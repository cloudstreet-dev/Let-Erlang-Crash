# Chapter 13: Links and Monitors: Watching Things Die

> *In Erlang, when a process dies alone in the forest, other processes absolutely hear it. That's the whole point. Links and monitors are the mechanisms that make "let it crash" actually work — they're how processes know that other processes have died, and how failure propagates through a system in a controlled way.*

---

## The Problem

You spawn a process to do some work. It crashes. Now what?

Without links or monitors, the answer is "nothing." The process is gone, nobody knows, and the work never gets done. Your system silently degrades. This is unacceptable.

Erlang gives you two mechanisms to detect process death: **links** and **monitors**.

## Links: Dying Together

A link is a bidirectional connection between two processes. If either one dies, the other one dies too.

```erlang
1> spawn_link(fun() -> exit(kaboom) end).
** exception exit: kaboom
```

Whoa! The spawned process exited, and it took *our shell process* down with it (the shell's supervisor restarted it). That's a link in action.

### Creating Links

```erlang
%% spawn_link: spawn and link in one atomic operation
Pid = spawn_link(fun() -> do_work() end).

%% Or link to an existing process
link(Pid).

%% Remove a link
unlink(Pid).
```

`spawn_link/1` is preferred over `spawn/1` + `link/1` because it's atomic — there's no window where the process exists but isn't linked.

### What Happens When a Linked Process Dies

```
Process A ←──link──→ Process B

If B dies with reason 'normal': A is unaffected
If B dies with reason 'kaboom': A also dies with reason 'kaboom'
```

Normal exits don't propagate through links. Only abnormal exits do. This means a process that finishes its work normally won't kill its linked partners.

### Trapping Exits

A process can choose to *trap* exit signals instead of dying:

```erlang
-module(trapper).
-export([start/0]).

start() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> exit(something_bad) end),
    receive
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p died: ~p~n", [Pid, Reason])
    end.
```

```erlang
1> trapper:start().
Process <0.91.0> died: something_bad
ok
```

When `trap_exit` is `true`, exit signals are converted to messages of the form `{'EXIT', Pid, Reason}`. The process can then decide what to do — restart the dead process, log the error, clean up resources, or propagate the failure.

This is exactly how supervisors work. A supervisor is a process that links to its children, traps exits, and restarts them when they die.

### Link Semantics Summary

```
Process dies with       | Linked process (not trapping) | Linked process (trapping)
------------------------|-------------------------------|-------------------------
exit(normal)            | Nothing happens               | Receives {'EXIT', Pid, normal}
exit(Reason)            | Also dies with Reason         | Receives {'EXIT', Pid, Reason}
error (runtime crash)   | Also dies                     | Receives {'EXIT', Pid, Reason}
exit(Pid, kill)         | Dies (unstoppable)            | Dies (unstoppable)
```

The `kill` reason is special — it cannot be trapped. `exit(Pid, kill)` is the "no really, die" signal. Use it with caution.

## Monitors: Watching Without Dying

Monitors are like links, but unidirectional and non-lethal. The monitoring process gets a notification when the monitored process dies, but doesn't die itself.

```erlang
1> Pid = spawn(fun() -> timer:sleep(1000), exit(oops) end).
<0.91.0>
2> Ref = monitor(process, Pid).
#Ref<0.1234567890.1234567890.123456>
3> receive Msg -> Msg after 2000 -> timeout end.
{'DOWN', #Ref<0.1234567890.1234567890.123456>, process, <0.91.0>, oops}
```

When the monitored process dies, the monitoring process receives a `{'DOWN', Ref, process, Pid, Reason}` message.

### Monitors vs. Links

| Feature | Links | Monitors |
|---------|-------|----------|
| Direction | Bidirectional | Unidirectional |
| On death | Linked process dies too (unless trapping) | Monitor receives a DOWN message |
| Setup | `link(Pid)` or `spawn_link(Fun)` | `monitor(process, Pid)` |
| Teardown | `unlink(Pid)` | `demonitor(Ref)` |
| Use case | Coupled processes that should die together | Watching processes you don't own |

### When to Use Which

**Use links when:**
- The processes are tightly coupled (they can't function without each other)
- You're building a supervisor
- You want failure to propagate

**Use monitors when:**
- You want to know when something dies but don't want to die with it
- You're implementing a client watching a server
- You need one-way failure notification

```erlang
%% Monitor pattern: start a worker, get notified when it's done (or dead)
do_async_work() ->
    {Pid, Ref} = spawn_monitor(fun() ->
        %% Do expensive computation
        Result = expensive_calculation(),
        exit({ok, Result})
    end),
    receive
        {'DOWN', Ref, process, Pid, {ok, Result}} ->
            {ok, Result};
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    after 30000 ->
        demonitor(Ref, [flush]),
        exit(Pid, kill),
        {error, timeout}
    end.
```

## Building a Simple Supervisor (By Hand)

Before we get to OTP supervisors (Chapter 17), let's build one from scratch to understand the concept:

```erlang
-module(my_supervisor).
-export([start/2]).

start(WorkerMod, WorkerArgs) ->
    spawn(fun() -> supervise(WorkerMod, WorkerArgs) end).

supervise(Mod, Args) ->
    process_flag(trap_exit, true),
    Pid = apply(Mod, start_link, Args),
    io:format("Supervisor: started worker ~p~n", [Pid]),
    receive
        {'EXIT', Pid, normal} ->
            io:format("Supervisor: worker exited normally~n"),
            ok;
        {'EXIT', Pid, Reason} ->
            io:format("Supervisor: worker crashed (~p), restarting~n",
                      [Reason]),
            supervise(Mod, Args)
    end.
```

This supervisor:
1. Traps exits
2. Starts a worker (linked, via `start_link`)
3. Waits for the worker to die
4. If the death was abnormal, restarts the worker
5. If the death was normal, does nothing

This is the essence of OTP supervisors, just without the battle-tested polish.

## Exit Signals in Detail

There are three kinds of exits:

```erlang
%% 1. Normal exit — function returns or exit(normal)
spawn(fun() -> ok end).  %% exits normally

%% 2. Abnormal exit — explicit or runtime error
spawn(fun() -> exit(badarg) end).
spawn(fun() -> 1/0 end).  %% runtime error

%% 3. External kill — another process sends an exit signal
exit(Pid, Reason).   %% Sends exit signal to Pid
exit(Pid, kill).     %% Unstoppable kill
```

The `exit/2` function sends an exit signal to another process. It's how supervisors kill children during shutdown.

## A Practical Example: Worker Pool

```erlang
-module(pool).
-export([start/1, work/2]).

start(Size) ->
    process_flag(trap_exit, true),
    Workers = [spawn_link(fun worker/0) || _ <- lists:seq(1, Size)],
    loop(Workers).

work(Pool, Task) ->
    Pool ! {work, self(), Task},
    receive {result, Result} -> Result end.

loop(Workers) ->
    receive
        {work, From, Task} ->
            case Workers of
                [W | Rest] ->
                    W ! {task, self(), From, Task},
                    loop(Rest);
                [] ->
                    self() ! {work, From, Task},  %% re-queue
                    loop([])
            end;
        {done, Worker, From, Result} ->
            From ! {result, Result},
            loop([Worker | Workers]);
        {'EXIT', Dead, _Reason} ->
            io:format("Worker ~p died, spawning replacement~n", [Dead]),
            New = spawn_link(fun worker/0),
            loop([New | (Workers -- [Dead])])
    end.

worker() ->
    receive
        {task, Pool, From, Task} ->
            Result = Task(),  %% Execute the task
            Pool ! {done, self(), From, Result},
            worker()
    end.
```

When a worker crashes, the pool detects it via the link, spawns a replacement, and keeps going. The pool never goes down, even if individual workers explode.

## Key Takeaways

- Links are bidirectional — if one dies, the other dies (unless trapping exits)
- Monitors are unidirectional — you get a notification, not a death sentence
- `trap_exit` converts exit signals into messages
- `spawn_link` and `spawn_monitor` are atomic versions of spawn + link/monitor
- Supervisors are just processes that trap exits and restart children
- `exit(Pid, kill)` is the unstoppable kill signal
- Links + trap_exit is the foundation of OTP supervision

Links and monitors are the plumbing that makes "let it crash" practical. Without them, process crashes would be silent failures. With them, failures become events that can be detected, logged, and recovered from automatically.

---

[← Previous: Let It Crash](12-let-it-crash.md) | [Next: Error Handling →](14-error-handling.md)
