# Chapter 23: Ports and NIFs: Talking to the Outside World

> *Erlang is great at concurrency, fault tolerance, and distributed systems. It's not great at number crunching, image processing, or calling that one C library your system absolutely depends on. That's fine. Erlang was never meant to do everything alone. Ports and NIFs are how the BEAM talks to the outside world — safely, efficiently, and without bringing down your system when the C code segfaults.*

---

## The Problem

Sometimes you need to:
- Call a C/C++ library (OpenSSL, image processing, etc.)
- Run an external program and communicate with it
- Use a Rust library for compute-heavy work
- Interface with system-level APIs

Erlang gives you three mechanisms, each with different safety/performance trade-offs:

| Mechanism | Safety | Speed | Isolation |
|-----------|--------|-------|-----------|
| **Ports** | Very safe | Moderate | Full (separate OS process) |
| **Port Drivers** | Risky | Fast | None (linked into BEAM) |
| **NIFs** | Risky | Fastest | None (runs in BEAM threads) |

## Ports: The Safe Way

A port runs an external program as a separate OS process. The BEAM communicates with it via stdin/stdout. If the external program crashes, the BEAM is unaffected.

### Running an External Program

```erlang
%% Open a port to the 'cat' command
Port = open_port({spawn, "cat"}, [binary]),

%% Send data to it
Port ! {self(), {command, <<"hello">>}},

%% Receive the result
receive
    {Port, {data, Data}} ->
        io:format("Got: ~s~n", [Data])
end,

%% Close the port
Port ! {self(), close}.
```

### A Practical Example: Calling Python

```erlang
-module(python).
-export([eval/1]).

eval(Expression) ->
    Cmd = io_lib:format("python3 -c \"print(~s)\"", [Expression]),
    Port = open_port({spawn, lists:flatten(Cmd)},
                     [binary, exit_status, stderr_to_stdout]),
    collect_output(Port, <<>>).

collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_output(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} ->
            {ok, string:trim(binary_to_list(Acc))};
        {Port, {exit_status, Code}} ->
            {error, {exit_code, Code, Acc}}
    after 10000 ->
        port_close(Port),
        {error, timeout}
    end.
```

```erlang
1> python:eval("2 ** 100").
{ok,"1267650600228229401496703205376"}
```

### Port Protocol

For persistent communication with an external program, use a length-prefixed protocol:

```erlang
Port = open_port({spawn, "./my_c_program"},
                 [{packet, 4}, binary]).
%% {packet, 4} means each message is prefixed with a 4-byte length header
```

The external program reads 4 bytes for length, then reads that many bytes for the message. This is how Erlang's own `ei` (Erlang Interface) library works for C programs.

### Port Characteristics

- **Isolated**: Runs in a separate OS process
- **Safe**: If the external program crashes, only the port dies
- **Supervised**: The port owner process can be supervised
- **Slower**: Data is serialized/copied between OS processes

## NIFs: The Fast Way

NIFs (Native Implemented Functions) are C (or Rust) functions that run directly inside the BEAM. They're called just like regular Erlang functions but execute native code.

### A Simple NIF

```c
/* my_nif.c */
#include "erl_nif.h"

static ERL_NIF_TERM
add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int a, b;
    enif_get_int(env, argv[0], &a);
    enif_get_int(env, argv[1], &b);
    return enif_make_int(env, a + b);
}

static ErlNifFunc nif_funcs[] = {
    {"add", 2, add}
};

ERL_NIF_INIT(my_nif, nif_funcs, NULL, NULL, NULL, NULL)
```

```erlang
%% my_nif.erl
-module(my_nif).
-export([add/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./my_nif", 0).

add(_, _) ->
    erlang:nif_error(nif_not_loaded).
```

Compile the C code:

```bash
gcc -shared -o my_nif.so -fPIC \
    -I$(erl -noshell -eval 'io:format("~s",[code:root_dir()])' -s init stop)/erts-*/include \
    my_nif.c
```

```erlang
1> my_nif:add(40, 2).
42
```

### The NIF Danger

Here's the thing about NIFs: **they run on the BEAM scheduler threads**. If a NIF takes too long or crashes, it affects the entire VM:

- **Long-running NIF**: Blocks a scheduler, causing latency spikes for all processes on that scheduler
- **Crashing NIF**: Takes down the entire BEAM VM (not just one process)
- **Memory leak in NIF**: Eats the BEAM's memory

This is why NIFs are called "the nuclear option." They break every safety guarantee the BEAM provides.

### Dirty NIFs

For NIFs that take more than 1ms, use dirty schedulers:

```c
static ErlNifFunc nif_funcs[] = {
    {"heavy_computation", 1, heavy_computation, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};
```

Dirty schedulers are separate thread pools that don't block the normal schedulers. There are two kinds:
- `ERL_NIF_DIRTY_JOB_CPU_BOUND` — For CPU-intensive work
- `ERL_NIF_DIRTY_JOB_IO_BOUND` — For IO-intensive work

### Yielding NIFs

For even better behavior, break long work into chunks:

```c
static ERL_NIF_TERM
long_computation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Do some work */
    if (not_done_yet) {
        /* Save state and yield */
        return enif_schedule_nif(env, "long_computation",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 long_computation_continue, argc, argv);
    }
    return result;
}
```

### Rustler: NIFs in Rust

Writing NIFs in C is error-prone. Rustler lets you write them in Rust with safety guarantees:

```rust
// native/my_nif/src/lib.rs
use rustler::{Encoder, Env, NifResult, Term};

#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

rustler::init!("Elixir.MyNif"); // Works with Erlang too
```

Rust NIFs still block schedulers and can still cause issues, but at least you won't have buffer overflows.

## Port Drivers: The Middle Ground

Port drivers are shared libraries (.so/.dll) loaded directly into the BEAM. They're faster than ports (no OS process overhead) but less safe than ports (crashes affect the BEAM).

Port drivers are largely deprecated in favor of NIFs. You'll encounter them in legacy code but shouldn't write new ones.

## Choosing the Right Mechanism

```
                     Safety ──────────────────→
                     ↑
                     │
                Speed│   NIFs
                     │     ↓
                     │   Port Drivers
                     │     ↓
                     │   Ports
                     │
```

**Use Ports when:**
- The external code is unreliable or untested
- Crash safety is more important than speed
- You're calling external programs (not libraries)
- You want full isolation

**Use NIFs when:**
- Performance is critical (microsecond-level)
- The code is well-tested and won't crash
- You need to call a C/Rust library directly
- Operations complete in under 1ms (or use dirty schedulers)

**In practice:**
- Most teams start with ports
- Switch to NIFs only when performance profiling shows they need to
- Use Rust (via Rustler) for NIFs when possible

## A Port-Based JSON Parser (Example)

```erlang
-module(json_port).
-export([start/0, parse/1, stop/0]).

start() ->
    Port = open_port({spawn, "./json_parser"}, [{packet, 4}, binary]),
    register(json_port, Port),
    ok.

parse(JsonBinary) ->
    json_port ! {self(), {command, JsonBinary}},
    receive
        {json_port, {data, Result}} ->
            binary_to_term(Result)
    after 5000 ->
        {error, timeout}
    end.

stop() ->
    json_port ! {self(), close}.
```

The external `json_parser` program reads JSON, parses it, converts to Erlang terms (using the `ei` library), and sends the result back. If it crashes, only the port dies — the BEAM keeps running.

## Key Takeaways

- Ports are safe (separate OS process) but slower
- NIFs are fast but dangerous (crash the whole VM, block schedulers)
- Dirty schedulers mitigate NIF blocking issues
- Rustler makes NIFs safer through Rust's memory safety
- Port drivers exist but are deprecated — use NIFs instead
- Start with ports, only move to NIFs when you have a proven performance need
- Always benchmark before choosing NIFs over ports

The BEAM's isolation model is its greatest strength. Ports preserve that isolation. NIFs break it. Choose wisely — and when you do use NIFs, test them thoroughly, because a segfault in a NIF doesn't crash a process. It crashes everything.

---

[← Previous: Distributed Erlang](22-distributed-erlang.md) | [Next: Testing →](24-testing.md)
