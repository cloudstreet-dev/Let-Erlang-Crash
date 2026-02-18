# Chapter 15: OTP: The Batteries That Are Actually Included

> *Every language claims to have "batteries included." Python has its standard library. Java has the JDK. JavaScript has... npm, I guess. But OTP isn't just batteries — it's the entire power grid. OTP is what turns Erlang from "interesting academic language" into "thing that runs half the world's telecom infrastructure." If you skip OTP, you've missed the point of Erlang entirely.*

---

## What Is OTP?

OTP stands for **Open Telecom Platform**. The name is historical — it was created at Ericsson for telecom systems. But OTP is not specific to telecom at all. It's a set of:

1. **Behaviours** — Design patterns implemented as framework modules (GenServer, Supervisor, etc.)
2. **Libraries** — Standard library modules for everything from cryptography to HTTP
3. **Tools** — Compiler, debugger, profiler, observer, release tools
4. **Principles** — Architectural guidelines for building robust systems

Think of OTP as the Rails of Erlang, except it's been battle-tested for decades and it ships with the language.

## Why OTP Matters

You *could* build everything from scratch with `spawn`, `send`, and `receive`. But you'd end up reinventing:

- Request-reply patterns (GenServer does this)
- Process supervision (Supervisor does this)
- State management (GenServer does this)
- Event handling (gen_event does this)
- State machines (gen_statem does this)
- Application lifecycle (application does this)
- Release management (release tools do this)

OTP codifies decades of experience into reusable patterns. Every serious Erlang project uses it.

## OTP Behaviours

A behaviour is like an interface in Java or a trait in Rust — it defines a set of callbacks your module must implement. But OTP behaviours also provide the generic part of the pattern.

The main behaviours:

| Behaviour | Purpose | You'll Use It... |
|-----------|---------|-----------------|
| `gen_server` | Client-server pattern | Constantly |
| `supervisor` | Process supervision | Constantly |
| `gen_statem` | Finite state machines | Sometimes |
| `gen_event` | Event handling/logging | Occasionally |
| `application` | Application lifecycle | For every app |

## The Callback Pattern

Every OTP behaviour works the same way:

1. The behaviour module (e.g., `gen_server`) implements the generic logic — receiving messages, managing state, handling calls
2. Your module implements the specific callbacks — what to do when a call comes in, what state to start with, how to handle casts

```
┌─────────────────────────┐
│     gen_server           │  ← Generic (OTP provides this)
│  - receive loop          │
│  - call/cast handling    │
│  - state threading       │
│  - error handling        │
│                          │
│  Calls YOUR callbacks:   │
│  ┌─────────────────────┐│
│  │  Your Module         ││  ← Specific (you write this)
│  │  - init/1            ││
│  │  - handle_call/3     ││
│  │  - handle_cast/2     ││
│  │  - handle_info/2     ││
│  │  - terminate/2       ││
│  └─────────────────────┘│
└─────────────────────────┘
```

You write the interesting parts. OTP handles the boring-but-critical parts.

## Your First OTP Application Structure

An OTP application has a standard directory layout:

```
my_app/
├── src/
│   ├── my_app.app.src          %% Application resource file
│   ├── my_app_app.erl          %% Application behaviour
│   ├── my_app_sup.erl          %% Top-level supervisor
│   └── my_app_server.erl       %% Your GenServer(s)
├── include/                     %% Header files (.hrl)
├── test/                        %% Tests
└── rebar.config                 %% Build configuration
```

Create a new project with rebar3:

```bash
rebar3 new app my_app
```

This generates the skeleton for you.

## The Application Resource File

`my_app.app.src` describes your application:

```erlang
{application, my_app, [
    {description, "My first OTP application"},
    {vsn, "0.1.0"},
    {registered, []},
    {mod, {my_app_app, []}},
    {applications, [
        kernel,
        stdlib
    ]},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, []}
]}.
```

The `{mod, {my_app_app, []}}` line tells OTP which module implements the `application` behaviour — the entry point.

## The Application Behaviour

```erlang
-module(my_app_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    my_app_sup:start_link().

stop(_State) ->
    ok.
```

That's the whole thing. `start/2` starts the top-level supervisor. `stop/1` cleans up. The application behaviour handles everything else.

## Starting Applications

```erlang
1> application:start(my_app).
ok
2> application:stop(my_app).
ok

%% Or start all dependencies too:
3> application:ensure_all_started(my_app).
{ok, [my_app]}
```

In production, applications are started as part of a release (we'll cover this later).

## OTP Applications You Already Use

When you start the Erlang shell, several OTP applications are already running:

```erlang
1> application:which_applications().
[{stdlib,"ERTS  CXC 138 10","5.0"},
 {kernel,"ERTS  CXC 138 10","9.0"}]
```

`kernel` and `stdlib` are OTP applications. So is `crypto`, `ssl`, `inets` (HTTP client/server), `mnesia`, `observer`, and dozens more. The entire Erlang ecosystem is built on OTP applications.

## Quick Preview: A Complete OTP App

Here's the full skeleton — don't worry about understanding every detail yet, we'll cover GenServer and Supervisors in the next chapters:

```erlang
%% my_app_sup.erl - Supervisor
-module(my_app_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => counter,
          start => {my_app_counter, start_link, []},
          restart => permanent,
          type => worker}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10},
          Children}}.
```

```erlang
%% my_app_counter.erl - GenServer
-module(my_app_counter).
-behaviour(gen_server).
-export([start_link/0, increment/0, get_count/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:cast(?MODULE, increment).

get_count() ->
    gen_server:call(?MODULE, get_count).

init([]) ->
    {ok, 0}.  %% Initial state is 0

handle_call(get_count, _From, Count) ->
    {reply, Count, Count};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(increment, Count) ->
    {noreply, Count + 1};
handle_cast(_Msg, State) ->
    {noreply, State}.
```

```erlang
1> application:ensure_all_started(my_app).
{ok,[my_app]}
2> my_app_counter:increment().
ok
3> my_app_counter:increment().
ok
4> my_app_counter:get_count().
2
```

A supervised, crash-resistant counter in about 40 lines of code. If the counter process crashes, the supervisor restarts it. If it crashes too many times, the supervisor shuts down (and *its* supervisor decides what to do).

## The OTP Design Principles

1. **Everything is a process** — State, computation, connections — all processes
2. **Processes are supervised** — Every process has a supervisor
3. **Applications are self-contained** — Each application manages its own process tree
4. **Behaviours encode patterns** — Don't reinvent the wheel
5. **Releases are deployable units** — Bundle applications into a deployable package

## Key Takeaways

- OTP is not optional — it's the framework that makes Erlang production-ready
- Behaviours (GenServer, Supervisor, etc.) encode battle-tested patterns
- You write callbacks; OTP handles the generic plumbing
- Applications have a standard structure: app module → supervisor → workers
- `rebar3` is the standard build tool
- Every serious Erlang project is built on OTP

The next three chapters will deep-dive into the most important OTP behaviours: GenServer (Chapter 16), Supervisors (Chapter 17), and Supervision Trees (Chapter 18). These three form the backbone of every Erlang system ever built.

---

[← Previous: Error Handling](14-error-handling.md) | [Next: GenServer →](16-genserver.md)
