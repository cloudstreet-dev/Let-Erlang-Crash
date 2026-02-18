# Chapter 21: Hot Code Reloading: Changing the Engine Mid-Flight

> *Imagine you're flying a plane at 30,000 feet and you need to replace an engine. In most programming languages, you land the plane, swap the engine, and take off again. In Erlang, you swap the engine while the plane is still flying. Passengers don't even feel it. This isn't a metaphor — Ericsson literally needed to upgrade telephone switches without dropping phone calls. So they built a language that supports it.*

---

## What Is Hot Code Reloading?

Hot code reloading (or "hot code upgrade") means loading new code into a running BEAM system without stopping it. Active processes continue running, connections stay open, and the new code takes effect immediately.

This is not:
- Blue-green deployment (running two versions and switching traffic)
- Rolling restarts (restarting instances one by one)
- Feature flags (enabling/disabling code paths)

This is literally replacing the code a running process uses *while it's running*.

## How It Works: Two Versions

The BEAM keeps two versions of each module in memory: the **current** version and the **old** version.

```
Module: my_server
┌─────────────────┐  ┌─────────────────┐
│  Current (v2)   │  │    Old (v1)     │
│  new code       │  │  previous code  │
│  new processes   │  │  old processes  │
│  use this       │  │  still running  │
└─────────────────┘  └─────────────────┘
```

- **New processes** use the current version
- **Existing processes** continue running the old version *until* they make a fully qualified function call (a call using `Module:Function(Args)`)
- When a process makes a fully qualified call, it switches to the current version
- When a *third* version is loaded, the old version is purged, and any process still running it is killed

## Simple Code Reloading

```erlang
%% In the shell:
1> c(my_module).       %% Compile and load
{ok,my_module}

2> l(my_module).       %% Just reload (already compiled)
{ok,my_module}

%% Or from the command line:
code:load_file(my_module).
code:purge(my_module).    %% Remove old version
```

## Why Fully Qualified Calls Matter

This is the key mechanism. Look at the difference:

```erlang
-module(counter).
-export([start/0, loop/1]).

start() ->
    spawn(?MODULE, loop, [0]).

%% Version 1: just counts
loop(N) ->
    receive
        increment ->
            loop(N + 1);         %% LOCAL call — stays on current version
        {get, From} ->
            From ! {count, N},
            ?MODULE:loop(N)      %% FULLY QUALIFIED — picks up new code!
    end.
```

The `?MODULE:loop(N)` call (which expands to `counter:loop(N)`) is fully qualified. When the BEAM executes it, it looks up the *current* version of `counter:loop/1`. If you've loaded new code, the process starts running it.

The `loop(N + 1)` call is local — it stays on whatever version the process is already running.

**Best practice**: Make at least one fully qualified call in your process loop so it can pick up upgrades.

OTP GenServers do this automatically. The generic `gen_server` code handles the loop, and when your module is reloaded, the next callback is dispatched to the new version.

## Reloading a GenServer

With OTP, hot code reloading is mostly automatic:

```erlang
-module(my_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, code_change/3]).

%% ... normal callbacks ...

%% This callback is called during code upgrade
code_change(OldVsn, State, _Extra) ->
    %% Transform state from old format to new format
    NewState = transform_state(OldVsn, State),
    {ok, NewState}.
```

The `code_change/3` callback lets you transform the process state when the code version changes. If your new code changes the state format, this is where you migrate it.

```erlang
%% Example: adding a field to state
code_change(_OldVsn, #{name := Name, count := Count}, _Extra) ->
    %% New version adds a 'history' field
    {ok, #{name => Name, count => Count, history => []}}.
```

## Release Upgrades (The Production Way)

For production systems, you don't just reload individual modules. You create **release upgrades** using OTP's release handling tools:

### 1. Define an appup file

```erlang
%% my_app.appup
{"2.0.0",
 [{"1.0.0", [{load_module, my_module}]}],      %% Upgrade instructions
 [{"1.0.0", [{load_module, my_module}]}]        %% Downgrade instructions
}.
```

### 2. Create a relup

```bash
rebar3 relup
```

### 3. Apply the upgrade

```erlang
release_handler:unpack_release("my_app-2.0.0").
release_handler:install_release("2.0.0").
release_handler:make_permanent("2.0.0").
```

The release handler:
1. Suspends affected processes
2. Loads new module code
3. Calls `code_change` on GenServers
4. Resumes processes
5. Updates the running release version

All while the system is running. Zero downtime.

## A Live Demo

Let's see it in action. Create this module:

```erlang
%% greeter.erl (version 1)
-module(greeter).
-export([start/0, loop/0]).

start() ->
    register(greeter, spawn(?MODULE, loop, [])).

loop() ->
    receive
        {greet, Name} ->
            io:format("Hello, ~s!~n", [Name]),
            ?MODULE:loop()  %% Fully qualified for hot reloading
    end.
```

```erlang
1> c(greeter).
{ok,greeter}
2> greeter:start().
true
3> greeter ! {greet, "Alice"}.
Hello, Alice!
```

Now edit the module (don't stop the shell):

```erlang
%% greeter.erl (version 2)
-module(greeter).
-export([start/0, loop/0]).

start() ->
    register(greeter, spawn(?MODULE, loop, [])).

loop() ->
    receive
        {greet, Name} ->
            io:format("Howdy, ~s! Welcome to Erlang!~n", [Name]),
            ?MODULE:loop()
    end.
```

Reload it:

```erlang
4> c(greeter).
{ok,greeter}
5> greeter ! {greet, "Alice"}.
Howdy, Alice! Welcome to Erlang!
```

The process is the same one (same PID, same registration). The code changed underneath it. No restart, no reconnection, no downtime.

## When Hot Code Reloading Breaks

It's not magic. Things can go wrong:

### State format changes

If v2 expects a different state format than v1, and you don't handle the migration in `code_change`, processes will crash when they try to use the old state with new code.

### Protocol changes

If v2 changes the message protocol, in-flight messages from v1 clients might not match the new `receive` patterns.

### Loading a third version

The BEAM only keeps two versions. If you load v3 while some processes are still on v1, those v1 processes are killed. Make sure everything has migrated to v2 before loading v3.

### Module dependencies

If module A depends on module B, and you update both, the load order matters. Update B first, then A.

## In Practice

Most Erlang teams today use rolling deployments rather than live code upgrades for major changes. Hot code reloading is still incredibly valuable for:

- **Debugging in production** — Load a module with extra logging, investigate, reload the original
- **Configuration changes** — Reload a config module without restarting
- **Minor fixes** — Patch a bug without dropping connections
- **Development** — Iterate fast without restarting the entire system

The Ericsson telecom switches that inspired Erlang used hot code upgrades to achieve legendary uptimes — some switches ran for years without a restart, through multiple software versions.

## Key Takeaways

- The BEAM keeps two versions of each module in memory simultaneously
- Fully qualified calls (`Module:Function()`) switch to the current version
- Local calls stay on the existing version
- OTP GenServers upgrade automatically via `code_change/3`
- Release upgrades are the production-grade approach
- Hot reloading is invaluable for debugging and minor fixes in production
- Be careful with state migrations and load order

Hot code reloading is one of Erlang's most distinctive features. No other mainstream platform supports it as deeply. It comes from the telecom heritage — if you can't take the switch down for maintenance, the maintenance has to happen while the switch is running.

---

[← Previous: Mnesia](20-mnesia.md) | [Next: Distributed Erlang →](22-distributed-erlang.md)
