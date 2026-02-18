# Chapter 18: Supervision Trees: It's Supervisors All the Way Down

> *A single supervisor watching a handful of workers is nice. A hierarchy of supervisors, each responsible for a section of your system, with failures propagating up and restarts cascading down — that's how you build systems that survive the real world. Supervision trees are the architectural pattern that makes Erlang systems feel immortal.*

---

## The Idea

A supervision tree is a hierarchy where:
- The root is an application supervisor
- Interior nodes are supervisors
- Leaves are worker processes
- Each level contains and manages the level below it

```
                    [App Supervisor]
                    /              \
          [DB Supervisor]     [Web Supervisor]
          /        \           /      |      \
     [Pool Sup]  [Cache]  [Listener] [Router] [Session Sup]
     / | | | \                                  / | | | \
    [W][W][W][W][W]                           [S][S][S][S][S]
```

Each supervisor only knows about its direct children. If a worker crashes, its immediate supervisor handles it. If the supervisor can't cope (too many restarts), *it* crashes, and *its* parent handles that. Failures escalate up; restarts cascade down.

## Designing a Supervision Tree

Let's design a supervision tree for a chat application:

```
                    [chat_app_sup]
                         |
           ┌─────────────┼──────────────┐
           |              |              |
     [chat_db_sup]  [chat_room_sup]  [chat_conn_sup]
           |              |              |
      ┌────┴────┐    (dynamic)      (dynamic)
      |         |    [room_1]       [conn_1]
   [db_pool] [cache] [room_2]      [conn_2]
    /||\      [room_3]              [conn_3]
   [w][w][w]   ...                   ...
```

**Why this structure?**

- Database crashes don't kill chat rooms (separate supervisors)
- One room crashing doesn't affect other rooms (dynamic children)
- Connection crashes don't affect rooms or the database
- If the entire DB subsystem is failing, `chat_db_sup` shuts down and `chat_app_sup` can decide what to do

## Building It

### Top-level supervisor

```erlang
-module(chat_app_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => chat_db_sup,
          start => {chat_db_sup, start_link, []},
          type => supervisor,
          shutdown => infinity},

        #{id => chat_room_sup,
          start => {chat_room_sup, start_link, []},
          type => supervisor,
          shutdown => infinity},

        #{id => chat_conn_sup,
          start => {chat_conn_sup, start_link, []},
          type => supervisor,
          shutdown => infinity}
    ],
    {ok, {#{strategy => one_for_one, intensity => 3, period => 60},
          Children}}.
```

### Database supervisor (static children)

```erlang
-module(chat_db_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => db_pool,
          start => {db_pool, start_link, [5]},  %% 5 connections
          restart => permanent,
          type => worker},
        #{id => cache,
          start => {chat_cache, start_link, []},
          restart => permanent,
          type => worker}
    ],
    %% rest_for_one: if the pool dies, restart cache too
    {ok, {#{strategy => rest_for_one, intensity => 5, period => 30},
          Children}}.
```

### Room supervisor (dynamic children)

```erlang
-module(chat_room_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, start_room/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_room(RoomName) ->
    supervisor:start_child(?MODULE, [RoomName]).

init([]) ->
    ChildSpec = #{
        id => chat_room,
        start => {chat_room, start_link, []},
        restart => transient,  %% Only restart on crashes, not normal exit
        type => worker
    },
    {ok, {#{strategy => simple_one_for_one, intensity => 10, period => 60},
          [ChildSpec]}}.
```

With `simple_one_for_one`, all children use the same spec. `start_child` appends its args to the spec's args:

```erlang
%% This calls chat_room:start_link(RoomName)
chat_room_sup:start_room("general").
chat_room_sup:start_room("random").
chat_room_sup:start_room("erlang-nerds").
```

## Failure Propagation

Here's where the tree structure pays off. Consider what happens when things fail:

### A single room crashes

```
1. Room "general" crashes
2. chat_room_sup detects it (link)
3. Since restart=transient and it was abnormal, restart it
4. New "general" room starts fresh
5. Nobody else is affected
```

### The database pool crashes repeatedly

```
1. db_pool crashes
2. chat_db_sup restarts it
3. db_pool crashes again (bad config?)
4. chat_db_sup restarts it again
5. db_pool crashes a third time
6. chat_db_sup exceeds intensity (3 in 30 seconds)
7. chat_db_sup shuts itself down
8. chat_app_sup detects this (link)
9. chat_app_sup restarts chat_db_sup (and all its children)
10. Fresh start — maybe the transient issue is resolved
```

### Everything is on fire

```
1. chat_db_sup keeps crashing after restart
2. chat_app_sup exceeds its intensity limit
3. chat_app_sup shuts down
4. The application stops
5. Maybe the node supervisor restarts the whole application
6. Or an operator is paged
```

The escalation is automatic and controlled. Each level handles what it can. What it can't handle, it escalates. There's no infinite loop of restarts — the intensity limits act as circuit breakers at every level.

## Design Principles

### 1. Separate things that fail independently

If component A failing shouldn't affect component B, they should be under different supervisors.

### 2. Group things that fail together

If components need each other to function, put them under the same supervisor with `one_for_all` or `rest_for_one`.

### 3. Keep supervisors thin

A supervisor with 50 children is a code smell. Split into sub-supervisors by functionality.

### 4. Choose restart strategies carefully

```
Independent children          → one_for_one
Tightly coupled children      → one_for_all
Sequential dependencies       → rest_for_one
Dynamic pool of same workers  → simple_one_for_one
```

### 5. Permanent vs. transient vs. temporary

```
Must always be running        → permanent
Should restart on crash       → transient
One-shot tasks               → temporary
```

## Visualizing Your Tree

The Observer tool shows your supervision tree graphically:

```erlang
1> observer:start().
```

Click the "Applications" tab to see the process tree for each running application. You can see PIDs, registered names, message queue lengths, and memory usage for every process. You can even kill processes to test your supervision strategy.

## A Complete Example: Job Queue

```erlang
%%% job_queue_app_sup.erl %%%
-module(job_queue_app_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => job_store,
          start => {job_store, start_link, []},
          restart => permanent,
          type => worker},
        #{id => job_worker_sup,
          start => {job_worker_sup, start_link, []},
          restart => permanent,
          type => supervisor,
          shutdown => infinity},
        #{id => job_scheduler,
          start => {job_scheduler, start_link, []},
          restart => permanent,
          type => worker}
    ],
    %% rest_for_one: scheduler depends on workers which depend on store
    {ok, {#{strategy => rest_for_one, intensity => 5, period => 60},
          Children}}.
```

```
[job_queue_app_sup] (rest_for_one)
├── [job_store]          - Persistent job storage (permanent)
├── [job_worker_sup]     - Dynamic worker pool (supervisor)
│   ├── [worker_1]       - Processes individual jobs (transient)
│   ├── [worker_2]
│   └── [worker_3]
└── [job_scheduler]      - Assigns jobs to workers (permanent)
```

If the store crashes, workers and scheduler restart (they need the store). If a worker crashes, only that worker restarts. If the scheduler crashes, only the scheduler restarts (workers keep running their current jobs).

## Key Takeaways

- Supervision trees are hierarchies of supervisors managing workers
- Failures propagate up; restarts cascade down
- Each supervisor level acts as a circuit breaker
- Design your tree around failure domains — what can fail independently?
- Use dynamic supervisors for pools of identical workers
- The Observer tool visualizes your supervision tree
- Keep supervisors thin and focused

The supervision tree is the architectural backbone of every Erlang system. It's what transforms "a bunch of processes" into "a system that heals itself." When people say Erlang has "nine nines" reliability, they're talking about well-designed supervision trees doing their job, automatically, 24/7, for years at a time.

---

[← Previous: Supervisors](17-supervisors.md) | [Next: ETS →](19-ets.md)
