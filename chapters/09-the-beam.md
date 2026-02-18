# Chapter 9: The BEAM: Erlang's Secret Weapon

> *People say "I use Erlang" but what they really mean is "I use the BEAM." The BEAM virtual machine is the actual magic. It's what makes millions of processes possible, what makes hot code reloading work, and what lets your system run for years without a restart. Erlang is great. The BEAM is the reason it's great.*

---

## What Is the BEAM?

BEAM stands for Bogdan/Bjorn's Erlang Abstract Machine (named after Bogumil "Bogdan" Hausman and Bjorn Gustavsson). It's a virtual machine — like the JVM for Java or V8 for JavaScript — but designed from the ground up for concurrency, fault tolerance, and soft real-time systems.

The BEAM is what runs your `.beam` files. When you compile `my_module.erl`, you get `my_module.beam` — bytecode that the BEAM executes.

But comparing the BEAM to the JVM is like comparing a submarine to a bicycle. They're both vehicles, technically, but they were designed for completely different environments.

## The BEAM's Design Goals

The BEAM was built to run telephone switches. That means:

1. **Massive concurrency** — Handle millions of simultaneous phone calls
2. **Soft real-time** — Every call gets fair service, no starvation
3. **Fault isolation** — One bad call can't crash the switch
4. **Hot upgrades** — Upgrade software without dropping calls
5. **High availability** — Run for years without downtime

These requirements shaped every design decision in the VM. Let's look at how.

## The Scheduler

The BEAM has a preemptive scheduler, and it's built around a concept called **reductions**.

Every process gets a budget of reductions (roughly one per function call). When a process uses up its budget, the scheduler suspends it and runs the next process. This happens transparently — no yield points, no async/await, no cooperative multitasking gymnastics.

```
Process A: |====|     |====|     |====|
Process B:      |====|     |====|     |====|
Process C: |==|   |==|   |==|   |==|   |==|
           ──────────────────────────────────→ time
```

By default, the BEAM runs one scheduler thread per CPU core. On an 8-core machine, 8 schedulers run in parallel, each with its own run queue of processes.

```erlang
%% How many schedulers are running?
1> erlang:system_info(schedulers_online).
8
```

This is why Erlang naturally uses all your cores. You don't have to think about threading or parallelism — the scheduler handles it.

## Process Architecture

Each BEAM process has:

- **Its own heap** — No shared memory between processes
- **Its own stack** — For function calls
- **Its own mailbox** — A queue for incoming messages
- **Its own garbage collector** — GC is per-process

A fresh process uses about 2.5 KB of memory. You can have millions of them:

```erlang
%% Spawn a million processes
1> Pids = [spawn(fun() -> receive stop -> ok end end)
1>         || _ <- lists:seq(1, 1_000_000)].
```

That just created a million processes, each waiting for a `stop` message. On a modern machine, this uses a few gigabytes of memory and takes a couple of seconds. Try that with OS threads.

## Per-Process Garbage Collection

This is huge and often overlooked. In most VMs (JVM, CLR, V8), garbage collection is global — the entire VM pauses to collect garbage. This causes latency spikes that make real-time systems impossible.

The BEAM does GC per-process. When Process A's heap fills up, only Process A pauses for GC. Every other process keeps running. The pause time is proportional to the size of that one process's heap, not the total heap of the system.

This is why Erlang systems have predictable latency. No global GC pauses, ever.

```
Process A: [running] [GC] [running]
Process B: [  running  running  running  ]  ← unaffected
Process C: [  running  running  running  ]  ← unaffected
```

Since messages between processes are copied (not shared), when a process dies, its entire heap is freed instantly. No reference counting, no cycle detection.

## The IO Scheduler

The BEAM has separate scheduler threads for IO-bound operations. When a process does file IO, network IO, or calls a NIF, it can be moved to an IO scheduler so it doesn't block the main compute schedulers.

This means you can have processes doing heavy IO alongside processes doing computation, and they don't interfere with each other.

## Memory Architecture

```
┌─────────────────────────────────────────────┐
│                  BEAM VM                     │
├──────────┬──────────┬──────────┬────────────┤
│Process A │Process B │Process C │  ...       │
│┌────────┐│┌────────┐│┌────────┐│            │
││  Heap  │││  Heap  │││  Heap  ││            │
│├────────┤│├────────┤│├────────┤│            │
││  Stack │││  Stack │││  Stack ││            │
│├────────┤│├────────┤│├────────┤│            │
││Mailbox │││Mailbox │││Mailbox ││            │
│└────────┘│└────────┘│└────────┘│            │
├──────────┴──────────┴──────────┴────────────┤
│              Shared Areas                    │
│  ┌──────────┐  ┌──────────┐  ┌───────────┐ │
│  │Atom Table│  │ETS Tables│  │Binary Heap │ │
│  └──────────┘  └──────────┘  └───────────┘ │
└─────────────────────────────────────────────┘
```

Each process has isolated memory. The only shared areas are:
- **Atom table** — All atoms, globally shared (this is why atoms aren't garbage collected)
- **ETS tables** — Shared in-memory tables (Chapter 19)
- **Large binaries** — Binaries over 64 bytes are reference-counted on a shared heap

## The JIT Compiler

Since OTP 24, the BEAM includes a JIT (Just-In-Time) compiler called BeamAsm. It compiles BEAM bytecode to native machine code at load time, giving significant performance improvements for compute-heavy code.

```erlang
%% Check if JIT is enabled
1> erlang:system_info(emu_flavor).
jit
```

The JIT is transparent — you don't need to change your code or compilation process. Your `.beam` files get JIT-compiled when they're loaded.

## Introspection: Watching the BEAM Work

The BEAM is remarkably transparent. You can observe everything:

```erlang
%% How many processes are running?
1> erlang:system_info(process_count).
57

%% Total memory usage
2> erlang:memory().
[{total,25518192},
 {processes,5765952},
 {system,19752240},
 {atom,354569},
 {binary,101432},
 {ets,384424},
 ...]

%% Info about a specific process
3> process_info(self()).
[{current_function,{erl_eval,do_apply,7}},
 {initial_call,{erlang,apply,2}},
 {status,running},
 {message_queue_len,0},
 {heap_size,4185},
 {stack_size,30},
 {reductions,12345},
 ...]
```

You can also use the built-in Observer GUI:

```erlang
1> observer:start().
```

This opens a graphical tool showing processes, memory, schedulers, ETS tables, and more. It's like `htop` on steroids, specifically for the BEAM.

## BEAM vs. The World

| Feature | BEAM | JVM | V8 |
|---------|------|-----|-----|
| Concurrency model | Lightweight processes | OS threads (virtual threads in JDK 21+) | Event loop + workers |
| GC scope | Per-process | Global (generational) | Global (generational) |
| Preemptive scheduling | Yes | Via OS | No (cooperative) |
| Hot code reload | Native | Hacky (class reloading) | No |
| Fault isolation | Process-level | Thread-level (limited) | No |
| Distribution | Built-in | Libraries (RMI, etc.) | Libraries |

## BEAM Languages

Erlang isn't the only language that runs on the BEAM:

- **Elixir** — Ruby-inspired syntax, same BEAM semantics. Hugely popular.
- **Gleam** — Statically typed, with a Rust-like syntax. Growing fast.
- **LFE** (Lisp Flavoured Erlang) — Lisp on the BEAM. For the adventurous.

All BEAM languages can call each other's modules directly. An Elixir project can use Erlang libraries and vice versa. The BEAM is the platform; languages are just syntax.

## Key Takeaways

- The BEAM is a virtual machine designed for concurrency, fault tolerance, and real-time systems
- Processes are incredibly lightweight (~2.5 KB each)
- The preemptive scheduler uses reductions, not cooperative yielding
- Per-process garbage collection eliminates global GC pauses
- One scheduler per CPU core — automatic parallelism
- The JIT compiler (BeamAsm) provides native-code performance
- The Observer tool lets you watch everything in real time
- The BEAM is a platform — Erlang, Elixir, and Gleam all run on it

The BEAM is why Erlang systems can handle millions of concurrent connections with predictable latency and 99.9999999% uptime. It's not magic — it's just very, very good engineering.

---

[← Previous: Recursion](08-recursion.md) | [Next: Processes →](10-processes.md)
