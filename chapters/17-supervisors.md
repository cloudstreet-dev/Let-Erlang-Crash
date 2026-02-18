# Chapter 17: Supervisors: Let Someone Else Worry

> *A supervisor is a process whose only job is to watch other processes and restart them when they die. That's it. That's the whole concept. It sounds simple because it is simple. And that simplicity is what lets Erlang systems achieve "nine nines" reliability — 99.9999999% uptime. Supervisors are the single most important architectural concept in OTP.*

---

## What Is a Supervisor?

A supervisor is an OTP process that:

1. Starts child processes
2. Monitors them (via links)
3. Restarts them when they crash
4. Reports on their status
5. Shuts them down cleanly when asked

That's the entire job description. Supervisors don't do business logic. They don't hold application state. They exist solely to keep other processes alive.

## Your First Supervisor

```erlang
-module(my_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        #{id => worker_1,
          start => {my_worker, start_link, []},
          restart => permanent,
          type => worker}
    ],
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    {ok, {SupFlags, ChildSpecs}}.
```

The `init/1` callback returns two things:

1. **Supervisor flags** — How the supervisor behaves (strategy, restart limits)
2. **Child specifications** — What processes to start and how

## Child Specifications

A child spec is a map that tells the supervisor everything it needs to know about a child:

```erlang
#{
    id => worker_1,            %% Unique identifier
    start => {Mod, Fun, Args}, %% How to start it: apply(Mod, Fun, Args)
    restart => permanent,      %% When to restart
    shutdown => 5000,          %% Shutdown timeout in ms (or brutal_kill)
    type => worker             %% worker or supervisor
}
```

### Restart strategies for children

| Value | Meaning |
|-------|---------|
| `permanent` | Always restart, even on normal exit |
| `transient` | Only restart on abnormal exit |
| `temporary` | Never restart |

```erlang
%% A database connection — always keep it alive
#{id => db_conn, restart => permanent, ...}

%% A one-off task — restart on crash, not on completion
#{id => batch_job, restart => transient, ...}

%% A request handler — let it go when it's done
#{id => request, restart => temporary, ...}
```

## Supervision Strategies

The strategy determines what happens when *one* child crashes:

### `one_for_one`

Only the crashed child is restarted. Other children are unaffected.

```
Before crash:    After B crashes:
[A] [B] [C]     [A] [B'] [C]
         ↓               ↑
       crash          restarted
```

Use when children are independent.

### `one_for_all`

All children are restarted when any one crashes.

```
Before crash:    After B crashes:
[A] [B] [C]     [A'] [B'] [C']
         ↓        ↑         ↑
       crash   all restarted
```

Use when children depend on each other and can't function alone.

### `rest_for_one`

The crashed child and all children started *after* it are restarted. Children started before it are left alone.

```
Before crash:    After B crashes:
[A] [B] [C]     [A] [B'] [C']
         ↓               ↑
       crash    B and C restarted (A untouched)
```

Use when children have sequential dependencies.

### `simple_one_for_one` (now deprecated in favor of dynamic supervisors)

All children are identical. Used for dynamic pools of workers.

## Restart Intensity: The Circuit Breaker

```erlang
SupFlags = #{
    strategy => one_for_one,
    intensity => 5,      %% Max 5 restarts...
    period => 10         %% ...within 10 seconds
}.
```

If the supervisor has to restart children more than `intensity` times in `period` seconds, it gives up and shuts itself down. This prevents infinite restart loops.

When a supervisor shuts down, *its* supervisor notices (because links) and can decide what to do — restart the whole subtree, escalate further, or shut down the application.

This is the circuit breaker pattern, built right into the framework.

## A Practical Example

Let's build a supervisor for a web service with a database connection and a cache:

```erlang
-module(web_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => database,
          start => {db_connection, start_link, ["localhost", 5432]},
          restart => permanent,
          shutdown => 5000,
          type => worker},

        #{id => cache,
          start => {cache_server, start_link, [#{max_size => 10000}]},
          restart => permanent,
          shutdown => 5000,
          type => worker},

        #{id => request_handler_sup,
          start => {request_handler_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,  %% Give child supervisor time to shut down
          type => supervisor}    %% This child is itself a supervisor
    ],
    {ok, {#{strategy => rest_for_one, intensity => 3, period => 60},
          Children}}.
```

We use `rest_for_one` because:
- If the database crashes, the cache and request handlers should restart (they depend on the DB)
- If the cache crashes, the request handlers should restart (they depend on the cache)
- If the request handler supervisor crashes, only it restarts (DB and cache are fine)

## Dynamic Children

You can add and remove children at runtime:

```erlang
%% Add a child
supervisor:start_child(my_sup, #{
    id => new_worker,
    start => {my_worker, start_link, [some_args]},
    restart => transient,
    type => worker
}).

%% Remove a child
supervisor:terminate_child(my_sup, new_worker).
supervisor:delete_child(my_sup, new_worker).

%% List all children
supervisor:which_children(my_sup).
%% [{worker_1, <0.89.0>, worker, [my_worker]},
%%  {new_worker, <0.95.0>, worker, [my_worker]}]

%% Count children
supervisor:count_children(my_sup).
%% [{specs, 2}, {active, 2}, {supervisors, 0}, {workers, 2}]
```

## Shutdown Sequence

When a supervisor shuts down, it stops children in reverse start order:

1. Sends `shutdown` signal to each child
2. Waits for `shutdown` timeout
3. If child doesn't stop, kills it

```erlang
%% Shutdown options:
shutdown => 5000        %% Wait 5 seconds, then kill
shutdown => infinity    %% Wait forever (use for supervisors)
shutdown => brutal_kill %% Kill immediately (no cleanup)
```

The `terminate/2` callback in a GenServer is called during shutdown, giving the process a chance to clean up.

## Inspecting Supervisors

```erlang
%% In the shell or observer:
1> supervisor:which_children(my_sup).
[{worker_1, <0.89.0>, worker, [my_worker]},
 {worker_2, <0.91.0>, worker, [my_worker]}]

2> supervisor:count_children(my_sup).
[{specs,2},{active,2},{supervisors,0},{workers,2}]

%% The Observer GUI shows a beautiful tree view:
3> observer:start().
```

## Key Takeaways

- Supervisors start, monitor, and restart child processes
- `one_for_one`: restart only the crashed child (most common)
- `one_for_all`: restart all children (tightly coupled children)
- `rest_for_one`: restart crashed child and everything after it
- Restart intensity limits prevent infinite restart loops
- `permanent` children always restart, `transient` only on crashes, `temporary` never
- Supervisors can be children of other supervisors (supervision trees)
- Use `shutdown => infinity` for child supervisors

Supervisors are boring. They don't do anything exciting. That's the point. The most critical part of your system should be the most boring. A supervisor is just a list of things to keep alive and rules for what to do when they die. Simple, reliable, battle-tested.

---

[← Previous: GenServer](16-genserver.md) | [Next: Supervision Trees →](18-supervision-trees.md)
