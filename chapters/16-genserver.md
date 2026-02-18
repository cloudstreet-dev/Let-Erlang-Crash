# Chapter 16: GenServer: Your New Best Friend

> *If Erlang had a greatest hits album, GenServer would be the lead single. It's the most-used OTP behaviour by a mile. Every stateful service, every connection handler, every cache, every worker — they're all GenServers. Once you understand it, you understand 60% of production Erlang code.*

---

## What Is a GenServer?

GenServer stands for "Generic Server." It's an OTP behaviour that implements the client-server pattern:

- **Server**: A process that holds state and responds to requests
- **Client**: Any process that sends requests to the server

GenServer handles all the boring stuff — the receive loop, message format, timeouts, error handling, code upgrades. You just implement the callbacks that say what the server *does*.

## The Callbacks

A GenServer module implements these callbacks:

| Callback | Called When | Must Return |
|----------|-----------|-------------|
| `init/1` | Server starts | `{ok, State}` |
| `handle_call/3` | Sync request (call) | `{reply, Reply, NewState}` |
| `handle_cast/2` | Async request (cast) | `{noreply, NewState}` |
| `handle_info/2` | Any other message | `{noreply, NewState}` |
| `terminate/2` | Server is shutting down | Ignored |

That's it. Five callbacks, and `terminate/2` is optional. Let's build something.

## A Key-Value Store

```erlang
-module(kv_store).
-behaviour(gen_server).

%% Public API
-export([start_link/0, put/2, get/1, delete/1, all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%%% Public API %%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

all() ->
    gen_server:call(?MODULE, all).

%%% Callbacks %%%

init([]) ->
    {ok, #{}}.  %% Initial state: empty map

handle_call({put, Key, Value}, _From, State) ->
    {reply, ok, State#{Key => Value}};

handle_call({get, Key}, _From, State) ->
    Reply = maps:get(Key, State, undefined),
    {reply, Reply, State};

handle_call(all, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({delete, Key}, State) ->
    {noreply, maps:remove(Key, State)};

handle_cast(_Msg, State) ->
    {noreply, State}.
```

```erlang
1> kv_store:start_link().
{ok,<0.89.0>}
2> kv_store:put(name, "Alice").
ok
3> kv_store:put(age, 30).
ok
4> kv_store:get(name).
"Alice"
5> kv_store:all().
#{age => 30,name => "Alice"}
6> kv_store:delete(age).
ok
7> kv_store:all().
#{name => "Alice"}
```

A complete, concurrent, crash-recoverable key-value store in 35 lines of callback code.

## call vs. cast

This is the most important distinction in GenServer:

### `gen_server:call/2` — Synchronous

```erlang
%% Client blocks until the server replies
Result = gen_server:call(Server, Request).
```

- The caller waits for a response
- Default timeout is 5 seconds
- Returns whatever the server puts in `{reply, Reply, State}`
- Use for queries and operations where you need the result

### `gen_server:cast/2` — Asynchronous

```erlang
%% Client sends and immediately continues
gen_server:cast(Server, Message).
```

- The caller doesn't wait — returns `ok` immediately
- No response from the server
- Use for fire-and-forget operations
- Be careful: you don't know if the server got the message or handled it

### When to Use Which

```erlang
%% CALL: You need the result
Balance = gen_server:call(account_server, {get_balance, UserId}).

%% CALL: You need confirmation it worked
ok = gen_server:call(db_server, {write, Key, Value}).

%% CAST: Fire and forget
gen_server:cast(logger, {log, info, "User logged in"}).

%% CAST: Performance-critical, don't need confirmation
gen_server:cast(metrics, {increment, page_views}).
```

## handle_info: Everything Else

`handle_info/2` catches messages that aren't calls or casts — things like:

- Messages from linked/monitored processes (`{'EXIT', ...}`, `{'DOWN', ...}`)
- Timer messages (`{timeout, ...}`)
- Raw messages sent with `Pid ! msg`
- Any other unexpected message

```erlang
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    io:format("Monitored process ~p died: ~p~n", [Pid, Reason]),
    {noreply, cleanup(Pid, State)};

handle_info(tick, State) ->
    %% periodic work
    do_periodic_stuff(),
    erlang:send_after(1000, self(), tick),
    {noreply, State};

handle_info(_Unexpected, State) ->
    %% Log and ignore unexpected messages
    {noreply, State}.
```

## Starting and Naming

```erlang
%% Local name (atom)
gen_server:start_link({local, my_server}, ?MODULE, Args, []).

%% Global name (across nodes)
gen_server:start_link({global, my_server}, ?MODULE, Args, []).

%% No name (use PID)
{ok, Pid} = gen_server:start_link(?MODULE, Args, []).

%% start vs start_link
gen_server:start(...)       %% Not linked to caller
gen_server:start_link(...)  %% Linked to caller (use in supervisors)
```

Use `start_link` when starting under a supervisor (which is almost always). The link lets the supervisor detect when the GenServer crashes.

## Timeouts

GenServer supports timeouts in several ways:

### Reply with timeout

```erlang
handle_call(something, _From, State) ->
    {reply, ok, State, 5000}.  %% Timeout after 5 seconds of inactivity
```

If no message arrives within 5000ms, `handle_info(timeout, State)` is called.

### Call timeout

```erlang
%% Client-side: wait at most 10 seconds for a reply
Result = gen_server:call(Server, Request, 10000).
```

### Self-scheduling with send_after

```erlang
init([]) ->
    erlang:send_after(1000, self(), tick),
    {ok, #{count => 0}}.

handle_info(tick, #{count := N} = State) ->
    io:format("Tick ~p~n", [N]),
    erlang:send_after(1000, self(), tick),
    {noreply, State#{count => N + 1}}.
```

## State Management Patterns

### Map state (most common)

```erlang
init([]) ->
    {ok, #{users => [], count => 0}}.

handle_call({add_user, User}, _From, #{users := Users, count := N} = State) ->
    {reply, ok, State#{users => [User | Users], count => N + 1}}.
```

### Record state

```erlang
-record(state, {connections = [], max_conns = 100, name}).

init([Name, MaxConns]) ->
    {ok, #state{name = Name, max_conns = MaxConns}}.

handle_call(status, _From, #state{connections = Conns, max_conns = Max} = State) ->
    {reply, #{active => length(Conns), max => Max}, State}.
```

## A Real-World Example: Rate Limiter

```erlang
-module(rate_limiter).
-behaviour(gen_server).
-export([start_link/2, allow/1]).
-export([init/1, handle_call/3, handle_info/2]).

start_link(Name, MaxPerSecond) ->
    gen_server:start_link({local, Name}, ?MODULE, MaxPerSecond, []).

allow(Name) ->
    gen_server:call(Name, allow).

init(MaxPerSecond) ->
    erlang:send_after(1000, self(), reset),
    {ok, #{max => MaxPerSecond, remaining => MaxPerSecond}}.

handle_call(allow, _From, #{remaining := 0} = State) ->
    {reply, {error, rate_limited}, State};
handle_call(allow, _From, #{remaining := N} = State) ->
    {reply, ok, State#{remaining => N - 1}}.

handle_info(reset, #{max := Max} = State) ->
    erlang:send_after(1000, self(), reset),
    {noreply, State#{remaining => Max}}.
```

```erlang
1> rate_limiter:start_link(api_limiter, 3).
{ok,<0.89.0>}
2> rate_limiter:allow(api_limiter).
ok
3> rate_limiter:allow(api_limiter).
ok
4> rate_limiter:allow(api_limiter).
ok
5> rate_limiter:allow(api_limiter).
{error,rate_limited}
%% Wait a second...
6> rate_limiter:allow(api_limiter).
ok
```

A production-quality rate limiter in 25 lines.

## Common Gotchas

### The bottleneck problem
A GenServer processes one message at a time. If your server is slow, calls queue up. Solutions:
- Keep handle_call fast
- Use cast for operations that don't need a reply
- Spawn a new process for expensive work
- Use a pool of GenServers

### Don't block in callbacks
```erlang
%% BAD: blocks the server for 30 seconds
handle_call(fetch_data, _From, State) ->
    Data = http:get("https://slow-api.com/data"),  %% 30 second timeout
    {reply, Data, State}.

%% GOOD: do expensive work in a separate process
handle_call(fetch_data, From, State) ->
    spawn(fun() ->
        Data = http:get("https://slow-api.com/data"),
        gen_server:reply(From, Data)
    end),
    {noreply, State}.
```

### Don't forget the catch-all clauses
Always add a catch-all clause for each callback to handle unexpected messages gracefully.

## Key Takeaways

- GenServer implements the client-server pattern with five callbacks
- `call` is synchronous (blocks), `cast` is asynchronous (fire-and-forget)
- `handle_info` catches non-call/cast messages
- State lives in the function argument — no mutable fields
- `start_link` for supervised processes, `start` for standalone
- Keep callbacks fast — spawn for expensive work
- GenServer is a single mailbox — it serializes all requests

GenServer is the workhorse of Erlang. Once you're comfortable with it, you can build almost anything — caches, connection pools, rate limiters, state machines, workers. It's the pattern you'll reach for first and use most often.

---

[← Previous: OTP](15-otp.md) | [Next: Supervisors →](17-supervisors.md)
