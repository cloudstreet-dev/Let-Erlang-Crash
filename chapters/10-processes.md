# Chapter 10: Processes: Lighter Than Your Threads

> *In Java, you might spawn 10,000 threads if you're feeling brave. In Go, maybe 100,000 goroutines. In Erlang, you spawn a million processes on your laptop during a coffee break and wonder what all the fuss was about. Erlang processes aren't threads. They're something better.*

---

## Spawning Your First Process

```erlang
1> spawn(fun() -> io:format("I'm alive! ~p~n", [self()]) end).
I'm alive! <0.89.0>
<0.89.0>
```

That's it. You just created a concurrent process. It ran, printed a message, and exited. The return value is its PID (Process Identifier) — `<0.89.0>`.

## What Is a Process?

An Erlang process is:
- A lightweight unit of execution managed by the BEAM
- Completely isolated (own heap, own stack, own mailbox)
- About 2.5 KB of initial memory
- Scheduled preemptively by the BEAM
- NOT an OS thread

Erlang processes are to OS threads what goldfish are to blue whales. Technically both are fish-related, but the comparison ends there.

## spawn/1 and spawn/3

Two ways to create processes:

```erlang
%% spawn/1 — takes a fun
Pid1 = spawn(fun() -> do_something() end).

%% spawn/3 — takes Module, Function, Args
Pid2 = spawn(my_module, my_function, [arg1, arg2]).
```

`spawn/3` is preferred for long-lived processes because it works with hot code reloading (the function reference is resolved at call time, not captured in a closure).

## Processes Are Concurrent

```erlang
-module(parallel).
-export([go/0]).

go() ->
    spawn(fun() -> count("Process A", 5) end),
    spawn(fun() -> count("Process B", 5) end),
    spawn(fun() -> count("Process C", 5) end),
    ok.

count(_, 0) -> done;
count(Name, N) ->
    io:format("~s: ~p~n", [Name, N]),
    count(Name, N - 1).
```

```erlang
1> parallel:go().
Process A: 5
Process B: 5
Process C: 5
Process A: 4
Process C: 4
Process B: 4
Process A: 3
...
```

The output is interleaved — three processes running concurrently, scheduled by the BEAM. The exact ordering varies between runs.

## Process Identity

Every process has a unique PID:

```erlang
1> self().
<0.85.0>
2> Pid = spawn(fun() -> timer:sleep(60000) end).
<0.91.0>
3> is_pid(Pid).
true
```

PIDs have three numbers: `<Node.ID1.ID2>`. For local processes, the node is always `0`. On distributed systems, it's different (Chapter 22).

## Registered Processes

You can give a process a name so you don't have to pass PIDs around:

```erlang
1> register(my_process, spawn(fun() ->
1>     receive Msg -> io:format("Got: ~p~n", [Msg]) end
1> end)).
true
2> my_process ! hello.
Got: hello
hello
3> whereis(my_process).
undefined  %% Process is dead now
```

`register/2` associates an atom with a PID. `whereis/1` looks it up. `registered/0` lists all registered names.

## Process Lifecycle

A process lives until:
1. Its function returns (normal exit)
2. It calls `exit(Reason)` explicitly
3. An unhandled error occurs
4. Another linked process crashes (Chapter 13)
5. Someone calls `exit(Pid, Reason)`

```erlang
%% Normal exit — function returns
spawn(fun() -> ok end).

%% Explicit exit
spawn(fun() -> exit(some_reason) end).

%% Error exit
spawn(fun() -> 1/0 end).
```

## Process State: The Receive Loop

Processes that need to maintain state do so through a recursive function with an accumulator:

```erlang
-module(counter).
-export([start/0, increment/1, get/1]).

start() ->
    spawn(fun() -> loop(0) end).

increment(Pid) ->
    Pid ! increment.

get(Pid) ->
    Pid ! {get, self()},
    receive
        {count, N} -> N
    end.

loop(Count) ->
    receive
        increment ->
            loop(Count + 1);
        {get, From} ->
            From ! {count, Count},
            loop(Count)
    end.
```

```erlang
1> C = counter:start().
<0.89.0>
2> counter:increment(C).
increment
3> counter:increment(C).
increment
4> counter:increment(C).
increment
5> counter:get(C).
3
```

The state (`Count`) lives in the function argument. Each recursive call carries the new state. No mutable variables, no shared memory, no locks. This is the fundamental Erlang design pattern.

## How Many Processes Can You Run?

Let's find out:

```erlang
-module(stress).
-export([test/1]).

test(N) ->
    Pids = [spawn(fun() -> receive stop -> ok end end)
            || _ <- lists:seq(1, N)],
    io:format("Spawned ~p processes~n", [length(Pids)]),
    io:format("Memory: ~p MB~n",
              [erlang:memory(total) div (1024 * 1024)]),
    %% Clean up
    [Pid ! stop || Pid <- Pids],
    ok.
```

```erlang
1> stress:test(100000).
Spawned 100000 processes
Memory: 586 MB
ok
2> stress:test(1000000).
Spawned 1000000 processes
Memory: 5765 MB
ok
```

A million processes in a few seconds. Each one is independently schedulable, has its own GC, and can send and receive messages. This is the BEAM's party trick.

## Process Information

You can inspect any process:

```erlang
1> process_info(self()).
[{current_function,{erl_eval,do_apply,7}},
 {initial_call,{erlang,apply,2}},
 {status,running},
 {message_queue_len,0},
 {heap_size,4185},
 {stack_size,30},
 {reductions,39186},
 ...]

%% Specific info
2> process_info(self(), message_queue_len).
{message_queue_len,0}
3> process_info(self(), heap_size).
{heap_size,4185}
```

This works on any process, not just `self()`. You can inspect running processes in production. Try doing that with OS threads.

## The Process Dictionary (Don't Use It)

Each process has a mutable dictionary. It exists. You shouldn't use it:

```erlang
1> put(key, "value").
undefined
2> get(key).
"value"
```

The process dictionary is mutable state — the one thing Erlang is designed to avoid. It makes code harder to reason about and test. It exists for historical reasons and some rare edge cases. Real Erlang code uses the receive-loop pattern for state.

If you see `put/get` in production code, someone made a choice. Whether it was a good choice is debatable.

## Key Takeaways

- `spawn/1` and `spawn/3` create new processes
- Processes are isolated, lightweight, and preemptively scheduled
- A million processes on a single machine is normal
- State is maintained through recursive receive loops, not mutable variables
- `register/2` gives processes names
- `process_info/1` lets you inspect any process
- Avoid the process dictionary

Processes are the fundamental unit of concurrency in Erlang. Everything else — message passing, links, monitors, supervisors, OTP — builds on this foundation. You don't write concurrent programs by adding threads to sequential code. You write programs *as* processes.

---

[← Previous: The BEAM](09-the-beam.md) | [Next: Message Passing →](11-message-passing.md)
