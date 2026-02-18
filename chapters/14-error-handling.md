# Chapter 14: Error Handling: It's Not What You Think

> *Erlang has try/catch. Yes, really. It's in the language. You can use it. Most experienced Erlang programmers barely touch it. This chapter explains why try/catch exists, when to actually use it, and why pattern matching and "let it crash" handle 90% of what try/catch does in other languages.*

---

## The Three Classes of Errors

Erlang has three kinds of errors:

### 1. Errors (`error`)
Runtime errors — things that go wrong during execution:

```erlang
1> 1/0.
** exception error: an error occurred when evaluating
                    an arithmetic expression
2> hd([]).
** exception error: bad argument
3> atom_to_list(123).
** exception error: bad argument
```

### 2. Exits (`exit`)
A process explicitly deciding to terminate:

```erlang
1> exit(reason).
** exception exit: reason
```

### 3. Throws (`throw`)
Non-local returns, used for flow control:

```erlang
1> throw(something).
** exception throw: something
```

Throws are the least common. They're occasionally used for deep escape from nested recursion, similar to how some languages use exceptions for control flow (which is usually frowned upon).

## try/catch

Yes, Erlang has it:

```erlang
try
    dangerous_operation()
catch
    error:Reason -> {error, Reason};
    exit:Reason -> {exit, Reason};
    throw:Value -> {thrown, Value}
end
```

With stack trace capture:

```erlang
try
    dangerous_operation()
catch
    Class:Reason:Stacktrace ->
        io:format("~p:~p~n~p~n", [Class, Reason, Stacktrace]),
        {error, Reason}
end
```

With an `after` clause (like `finally` in Java):

```erlang
try
    File = open_file(),
    process(File)
catch
    _:_ -> error
after
    close_file(File)  %% Always runs
end
```

## When to Use try/catch

**Use it at system boundaries:**

```erlang
%% Parsing user input — this is expected to fail sometimes
parse_user_input(Input) ->
    try
        {ok, list_to_integer(Input)}
    catch
        error:badarg -> {error, not_a_number}
    end.

%% External API calls — the outside world is hostile
call_external_service(Request) ->
    try
        httpc:request(Request)
    catch
        error:_ -> {error, service_unavailable}
    end.
```

**Use it for cleanup:**

```erlang
with_file(Path, Fun) ->
    {ok, Fd} = file:open(Path, [read]),
    try
        Fun(Fd)
    after
        file:close(Fd)
    end.
```

## When NOT to Use try/catch

Most of the time. Seriously.

### Don't catch errors you can't recover from:

```erlang
%% BAD: catching everything and hiding the problem
process_data(Data) ->
    try
        do_complex_processing(Data)
    catch
        _:_ -> {error, "something went wrong"}
    end.
%% This hides the real error. When things break in production,
%% you'll have no idea why.
```

### Don't use it for control flow:

```erlang
%% BAD: using exceptions for expected conditions
find_user(Id) ->
    try
        Users = get_all_users(),
        lists:keyfind(Id, 1, Users)
    catch
        error:_ -> not_found
    end.

%% GOOD: use pattern matching
find_user(Id) ->
    case get_all_users() of
        {ok, Users} ->
            case lists:keyfind(Id, 1, Users) of
                false -> {error, not_found};
                User -> {ok, User}
            end;
        {error, _} = Error ->
            Error
    end.
```

### Don't wrap internal code in try/catch:

```erlang
%% BAD: defensive try/catch around internal functions
handle_request(Req) ->
    try
        User = get_user(Req),
        Order = create_order(User, Req),
        {ok, Order}
    catch
        _:_ -> {error, internal_error}
    end.

%% GOOD: let pattern matching handle it, let crashes happen
handle_request(Req) ->
    {ok, User} = get_user(Req),
    {ok, Order} = create_order(User, Req),
    {ok, Order}.
%% If get_user or create_order fails, this process crashes.
%% The supervisor restarts it. The error is logged with full details.
```

## The `{ok, Value} | {error, Reason}` Convention

Erlang's primary error-handling mechanism isn't try/catch — it's return values:

```erlang
case file:open("config.txt", [read]) of
    {ok, Fd} ->
        Data = read_config(Fd),
        file:close(Fd),
        {ok, Data};
    {error, enoent} ->
        {ok, default_config()};
    {error, Reason} ->
        {error, {config_read_failed, Reason}}
end
```

This pattern is everywhere in Erlang. Functions that can fail return `{ok, Value}` or `{error, Reason}`. The caller decides what to do:

- Match on `{ok, Value}` — crash if it's not ok (assertive style)
- Case on both possibilities — handle the error explicitly
- Pass the error up — return `{error, Reason}` to your caller

```erlang
%% Assertive: crash if not ok
{ok, Config} = read_config("app.conf").

%% Explicit: handle both cases
case read_config("app.conf") of
    {ok, Config} -> start_with(Config);
    {error, _} -> start_with(defaults())
end.
```

## Error Handling Patterns

### Pattern 1: Assert and Crash

```erlang
%% If this isn't a valid user, something is very wrong
{ok, #user{role = admin}} = get_user(AdminId).
```

### Pattern 2: Handle Expected Errors

```erlang
%% File might not exist — that's expected
case file:read_file(Path) of
    {ok, Data} -> process(Data);
    {error, enoent} -> create_default_file(Path)
end.
```

### Pattern 3: Propagate Errors

```erlang
%% Let the caller deal with it
read_and_parse(Path) ->
    case file:read_file(Path) of
        {ok, Data} -> parse(Data);
        {error, _} = Error -> Error
    end.
```

### Pattern 4: Resource Cleanup

```erlang
%% try/after is appropriate here
with_connection(Fun) ->
    {ok, Conn} = connect(),
    try
        Fun(Conn)
    after
        disconnect(Conn)
    end.
```

## The catch Expression (Old Style)

There's an older form of catch that you'll see in legacy code:

```erlang
1> catch 1/0.
{'EXIT',{badarith,[{erlang,'/',[1,0],[]}|...]}}
2> catch throw(hello).
hello
3> catch exit(reason).
{'EXIT',reason}
```

The bare `catch` wraps the result in `{'EXIT', Reason}` for errors and exits, and returns the value directly for throws. It's less precise than `try/catch` and mostly replaced by it. You don't need to use it, but you should recognize it in older codebases.

## The Big Picture

```
┌─────────────────────────────────────────────────────┐
│              Error Handling Strategy                  │
│                                                      │
│  Layer 1: Pattern Matching                           │
│  ─────────────────────                               │
│  {ok, Value} = risky_operation()                     │
│  → Crashes on error. Supervisor restarts.            │
│  → Use for internal code you trust.                  │
│                                                      │
│  Layer 2: Case Expressions                           │
│  ────────────────────                                │
│  case risky_operation() of                           │
│      {ok, V} -> use(V);                              │
│      {error, R} -> handle(R)                         │
│  end                                                 │
│  → Explicit handling. Use for expected error cases.  │
│                                                      │
│  Layer 3: try/catch                                  │
│  ──────────────────                                  │
│  try ... catch ... end                               │
│  → System boundaries, cleanup, external calls.       │
│                                                      │
│  Layer 4: Supervisors                                │
│  ────────────────────                                │
│  Automatic restart on crash.                         │
│  → The safety net for everything else.               │
└─────────────────────────────────────────────────────┘
```

Most Erlang code uses Layers 1 and 2. Layer 3 appears at boundaries. Layer 4 is always there, catching what falls through.

## Key Takeaways

- Erlang has three error classes: `error`, `exit`, `throw`
- `try/catch` exists but is used sparingly
- The `{ok, Value} | {error, Reason}` convention is the primary error mechanism
- Pattern match on `{ok, Value}` to assert success (crash on failure)
- Use `case` to handle expected error conditions
- Use `try/catch` at system boundaries and for resource cleanup
- Don't catch errors you can't meaningfully recover from
- Supervisors are Layer 4 — the safety net for unexpected failures

In Erlang, error handling isn't about preventing crashes. It's about building systems where crashes are harmless. The combination of pattern matching, return-value conventions, and supervision creates a layered defense that's simpler and more robust than try/catch-everything.

---

[← Previous: Links and Monitors](13-links-and-monitors.md) | [Next: OTP →](15-otp.md)
