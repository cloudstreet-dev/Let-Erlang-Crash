# Chapter 12: Let It Crash: The Philosophy

> *This is the chapter. The one that gives the book its name. "Let it crash" is the most famous idea in Erlang, and also the most misunderstood. People hear it and think it means "don't handle errors" or "write sloppy code." It means the exact opposite. It means building systems so well-designed that individual failures don't matter.*

---

## The Conventional Wisdom Is Wrong

In most programming traditions, you're taught:

1. Anticipate everything that could go wrong
2. Write defensive code for every possible failure
3. Catch every exception
4. Never let anything crash

This sounds reasonable. It's also impossible.

You can't anticipate every failure. Hardware fails in ways you've never seen. Networks do things that violate the spec. Users input data you couldn't imagine. Race conditions manifest once in a billion runs. Your dependencies have bugs.

Defensive programming says "prevent all crashes." Erlang says "crashes are inevitable, so build systems that handle them gracefully."

## What "Let It Crash" Actually Means

It does NOT mean:

- Write careless code
- Ignore errors
- Don't validate inputs
- Skip testing
- YOLO

It DOES mean:

- **Don't try to handle errors you can't meaningfully recover from.** If a process encounters an unexpected state, let it crash. A fresh restart is better than limping along with corrupted data.
- **Separate error handling from business logic.** The process doing the work shouldn't also be responsible for recovering from its own failure. That's someone else's job (a supervisor).
- **Processes are cheap and disposable.** Unlike threads or OS processes, Erlang processes are meant to be created and destroyed constantly.
- **Start clean.** When something goes wrong, the best recovery is often to restart from a known good state. A fresh process has no corrupted state by definition.

## The Coffee Machine Analogy

Imagine you're running a coffee shop with a fancy espresso machine.

**The defensive programming approach:** You put sensors on everything. Temperature sensor, pressure sensor, water level sensor, grind quality sensor. When something's wrong, the machine tries to compensate — adjusting temperature, changing pressure, switching water sources. The machine has 10,000 lines of error-handling code. It almost never crashes. But when it does, it's spectacular, and nobody knows how to fix it because the error-handling code has error-handling code.

**The Erlang approach:** The espresso machine makes coffee. If anything goes wrong — bad temperature, low pressure, whatever — the machine stops, dumps the bad shot, and starts over with a fresh attempt. A supervisor (the barista) watches the machine. If it fails three times in a row, the barista calls the repair service. The machine's code is simple. The failure handling is separate. And the customer still gets their coffee.

## Defensive vs. Offensive Programming

Consider reading a configuration file:

### The Defensive Way (Other Languages)

```python
def read_config(path):
    try:
        with open(path, 'r') as f:
            data = f.read()
    except FileNotFoundError:
        logger.warning(f"Config not found: {path}")
        return default_config()
    except PermissionError:
        logger.error(f"Permission denied: {path}")
        return default_config()
    except IOError as e:
        logger.error(f"IO error: {e}")
        return default_config()

    try:
        config = parse_json(data)
    except JSONDecodeError as e:
        logger.error(f"Invalid JSON: {e}")
        return default_config()

    if 'database' not in config:
        logger.warning("Missing database config")
        config['database'] = default_db_config()
    if 'port' not in config.get('database', {}):
        config['database']['port'] = 5432

    return config
```

### The Erlang Way

```erlang
read_config(Path) ->
    {ok, Data} = file:read_file(Path),
    {ok, Config} = parse_json(Data),
    #{database := #{port := _}} = Config,
    Config.
```

If the file doesn't exist, the match fails and the process crashes. If the JSON is invalid, the match fails and the process crashes. If the config is missing required fields, the match fails and the process crashes.

The supervisor restarts the process, which tries again (maybe the file was being written). If it keeps failing, the supervisor escalates. The error gets logged automatically. The stack trace tells you exactly what went wrong.

The defensive version has 20+ lines of error handling. The Erlang version has 4 lines of business logic. And the Erlang version is *more robust*, because it doesn't silently mask errors with defaults.

## The Key Insight: Separation of Concerns

The revolutionary idea isn't "let things crash." It's "the thing doing the work shouldn't also be responsible for fixing itself."

```
┌──────────────┐
│  Supervisor  │  ← Watches for failures, decides recovery strategy
│              │
│  ┌────────┐  │
│  │Worker A│  │  ← Does work. If confused, crashes. Gets restarted.
│  └────────┘  │
│  ┌────────┐  │
│  │Worker B│  │  ← Same. Just does its job.
│  └────────┘  │
│  ┌────────┐  │
│  │Worker C│  │  ← Ditto.
│  └────────┘  │
└──────────────┘
```

Worker processes are simple. They handle the happy path. When something unexpected happens, they crash. The supervisor notices, logs the crash, and restarts the worker. This is a fundamentally different architecture from try/catch-everything.

## When Should a Process Crash?

Crash on:
- Unexpected input that violates assumptions
- Corrupted internal state
- Failed assertions (pattern match failures)
- Unexpected error returns from dependencies
- Anything you can't meaningfully recover from *within* the process

Don't crash on:
- Expected error conditions (user not found, file doesn't exist *yet*)
- Validation errors you can report back to callers
- Temporary failures you can retry

```erlang
%% DO let this crash — bad state, can't recover
process_order(#{items := [], total := Total}) when Total > 0 ->
    %% Empty order with a total? Something is very wrong.
    %% Don't try to fix it. Crash. Let the supervisor sort it out.
    error({corrupted_order, empty_items_nonzero_total}).

%% DON'T crash on this — expected business logic
find_user(UserId) ->
    case db:lookup(users, UserId) of
        {ok, User} -> {ok, User};
        not_found -> {error, user_not_found}
    end.
```

## The Error Kernel Pattern

The "error kernel" is the minimal core of your system that *must not fail*. Everything else can crash and be restarted.

```
┌──────────────────────────────────────────┐
│              Error Kernel                 │
│  (Supervisors, critical config, etc.)    │
│  This code is simple, well-tested,       │
│  and handles failure explicitly.          │
│                                          │
│  ┌────────────────────────────────────┐  │
│  │        Everything Else             │  │
│  │  (Workers, connections, handlers)  │  │
│  │  This code does the actual work    │  │
│  │  and is allowed to crash.          │  │
│  └────────────────────────────────────┘  │
└──────────────────────────────────────────┘
```

The error kernel is small, boring, and rock-solid. The rest of the system is where the exciting (and crash-prone) stuff happens.

## Real-World Impact

Why does this work in practice?

1. **Simpler code.** Workers don't have defensive error handling cluttering up the business logic.
2. **Better error reports.** A crash with a stack trace tells you exactly what happened. A silently-caught exception with a fallback default might hide the real problem for months.
3. **Automatic recovery.** The supervisor restarts the process immediately. For many transient failures (network blips, temporary resource exhaustion), a restart is all you need.
4. **No corrupted state.** A restarted process starts fresh. No half-updated data, no stale caches, no zombie state.
5. **Escalation.** If restarts don't help, the supervisor can escalate — restart a group of processes, or shut down a subsystem. The failure propagates up the supervision tree until something can handle it.

## A Mental Model

Think of it this way:

| Traditional | Erlang |
|------------|--------|
| Try to prevent all errors | Accept that errors happen |
| Handle errors where they occur | Handle errors in a supervisor |
| Keep the process alive at all costs | Let it crash, restart fresh |
| Complex error-handling code | Simple workers, simple supervisors |
| Errors are exceptional | Crashes are routine |
| Defensive programming | Offensive programming |

## Key Takeaways

- "Let it crash" means separating error recovery from business logic
- Processes that encounter unexpected states should crash, not limp along
- Supervisors handle the recovery — not the process itself
- A fresh restart is often the best recovery strategy
- The error kernel pattern keeps your critical code simple and reliable
- This isn't sloppy — it's a deliberate architecture for building reliable systems
- The result is simpler code that's paradoxically *more* robust

"Let it crash" is not a motto for lazy programming. It's a design principle for building systems that survive the real world. It works because the BEAM makes processes cheap, isolation real, and recovery automatic.

---

[← Previous: Message Passing](11-message-passing.md) | [Next: Links and Monitors →](13-links-and-monitors.md)
