# Chapter 19: ETS: Stupid Fast In-Memory Storage

> *Sometimes a GenServer holding a map isn't fast enough. Sometimes you need multiple processes to read the same data concurrently without bottlenecking on a single mailbox. Sometimes you need to store a million records and look them up in microseconds. That's ETS — Erlang Term Storage. It's an in-memory key-value store built right into the VM, and it's terrifyingly fast.*

---

## What Is ETS?

ETS (Erlang Term Storage) is a built-in facility for storing large amounts of data in memory. Think of it as a shared, concurrent hash table (or ordered set) that lives inside the BEAM VM.

Key properties:
- **In-memory** — No disk, no network, just RAM
- **Constant-time** lookups for hash-based tables
- **Concurrent reads** — Multiple processes can read simultaneously
- **Destructive updates** — Unlike normal Erlang data, ETS is mutable
- **No garbage collection overhead** — Data lives outside the process heap

## Creating a Table

```erlang
1> Tab = ets:new(my_table, [set, named_table, public]).
my_table
```

Options breakdown:
- `set` — Table type (see below)
- `named_table` — Access by name instead of table ID
- `public` — Any process can read/write

### Table Types

| Type | Duplicates | Order | Lookup |
|------|-----------|-------|--------|
| `set` | No (unique keys) | None | O(1) hash |
| `ordered_set` | No (unique keys) | Sorted by key | O(log n) tree |
| `bag` | Yes (unique {key,value} pairs) | None | O(1) hash |
| `duplicate_bag` | Yes (full duplicates allowed) | None | O(1) hash |

`set` is the most common. Use `ordered_set` when you need range queries.

### Access Modes

| Mode | Who can read | Who can write |
|------|-------------|--------------|
| `public` | Any process | Any process |
| `protected` | Any process | Owner only |
| `private` | Owner only | Owner only |

Default is `protected`. Use `public` when multiple processes need to write.

## Basic Operations

```erlang
%% Create a table
ets:new(users, [set, named_table, public]).

%% Insert
ets:insert(users, {1, "Alice", 30}).
ets:insert(users, {2, "Bob", 25}).
ets:insert(users, {3, "Carol", 35}).

%% Lookup
ets:lookup(users, 1).
%% [{1, "Alice", 30}]

ets:lookup(users, 99).
%% []

%% Delete
ets:delete(users, 2).

%% Delete the whole table
ets:delete(users).
```

Note: `lookup` always returns a list (empty if not found). For `set` and `ordered_set`, it's always a zero or one element list.

## Inserting and Updating

```erlang
%% Insert a single tuple
ets:insert(cache, {key1, "value1"}).

%% Insert multiple tuples at once
ets:insert(cache, [{key2, "value2"}, {key3, "value3"}]).

%% Insert only if key doesn't exist
ets:insert_new(cache, {key1, "won't overwrite"}).
%% false (key1 already exists)

%% Atomic counter update
ets:update_counter(stats, page_views, 1).
%% Increments the counter atomically — no race conditions!
```

`update_counter` is particularly useful — it's an atomic increment that doesn't require a GenServer bottleneck.

## Pattern Matching with match and select

### ets:match — Simple pattern matching

```erlang
%% Setup
ets:new(people, [set, named_table]).
ets:insert(people, [
    {1, "Alice", 30, engineer},
    {2, "Bob", 25, designer},
    {3, "Carol", 35, engineer},
    {4, "Dave", 28, manager}
]).

%% Find all engineers (return matched variables)
ets:match(people, {'_', '$1', '_', engineer}).
%% [["Alice"], ["Carol"]]

%% '$1', '$2', etc. are match variables that get returned
%% '_' matches anything but discards the value
```

### ets:match_object — Return full objects

```erlang
ets:match_object(people, {'_', '_', '_', engineer}).
%% [{1, "Alice", 30, engineer}, {3, "Carol", 35, engineer}]
```

### ets:select — The Power Tool

`ets:select` uses match specifications — like SQL WHERE clauses:

{% raw %}
```erlang
%% Find people over 30
ets:select(people, [
    {{'$1', '$2', '$3', '$4'},        %% Pattern
     [{'>', '$3', 30}],                %% Guard (age > 30)
     ['$2']}                           %% Return (just name)
]).
%% ["Carol"]

%% ets:fun2ms helps build match specs from fun syntax
ets:select(people,
    ets:fun2ms(fun({_Id, Name, Age, _Role}) when Age > 28 ->
        {Name, Age}
    end)).
%% [{"Alice", 30}, {"Carol", 35}]
```
{% endraw %}

`ets:fun2ms` is a parse transform that converts a fun into a match specification at compile time. Much more readable.

## ETS vs. GenServer State

When should you use ETS instead of a GenServer holding a map?

### Use a GenServer when:
- Data is small (hundreds to thousands of entries)
- Access patterns are simple
- You need complex update logic
- Single-writer is fine

### Use ETS when:
- Data is large (millions of entries)
- Multiple processes need concurrent read access
- You need atomic counters
- Read performance is critical
- You want to avoid process bottlenecks

```
GenServer:  Process A ──→ [GenServer] ──→ Process B
            (serialized through one mailbox)

ETS:        Process A ──→ [ETS Table] ←── Process B
            (concurrent reads, no bottleneck)
```

## ETS and Process Ownership

Every ETS table has an owner process. When the owner dies, the table is destroyed. This is a common trap:

```erlang
%% BAD: table dies when the shell process crashes
1> ets:new(my_data, [named_table, public]).
2> %% ... shell crashes ... table is gone!
```

Solution: create the table in a supervised process:

```erlang
-module(table_owner).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(my_data, [set, named_table, public]),
    {ok, #{}}.

handle_info(_Msg, State) ->
    {noreply, State}.
```

You can also use `ets:give_away/3` to transfer ownership, or set an `heir` option so the table transfers automatically on owner death:

```erlang
ets:new(my_data, [set, named_table, public,
                   {heir, HeirPid, heir_data}]).
```

## Performance Characteristics

ETS is fast. Really fast:

```erlang
%% Benchmark: lookup 1 million times
bench() ->
    Tab = ets:new(bench, [set]),
    ets:insert(Tab, {key, "value"}),
    {Time, _} = timer:tc(fun() ->
        [ets:lookup(Tab, key) || _ <- lists:seq(1, 1000000)]
    end),
    ets:delete(Tab),
    io:format("~p lookups/sec~n", [1000000 * 1000000 div Time]).
```

On modern hardware, you'll see millions of lookups per second. ETS tables use the BEAM's memory allocators directly, bypassing per-process GC entirely.

## Practical Example: A Session Store

{% raw %}
```erlang
-module(sessions).
-export([init/0, create/1, get/1, touch/1, expire_old/1]).

init() ->
    ets:new(sessions, [set, named_table, public,
                       {read_concurrency, true}]).

create(UserId) ->
    SessionId = base64:encode(crypto:strong_rand_bytes(32)),
    ets:insert(sessions, {SessionId, UserId, erlang:system_time(second)}),
    SessionId.

get(SessionId) ->
    case ets:lookup(sessions, SessionId) of
        [{SessionId, UserId, _LastAccess}] -> {ok, UserId};
        [] -> {error, not_found}
    end.

touch(SessionId) ->
    case ets:lookup(sessions, SessionId) of
        [{SessionId, UserId, _}] ->
            ets:insert(sessions, {SessionId, UserId, erlang:system_time(second)}),
            ok;
        [] ->
            {error, not_found}
    end.

expire_old(MaxAgeSecs) ->
    Cutoff = erlang:system_time(second) - MaxAgeSecs,
    ets:select_delete(sessions, [
        {{'_', '_', '$1'}, [{'<', '$1', Cutoff}], [true]}
    ]).
```
{% endraw %}

Concurrent reads, fast lookups, no GenServer bottleneck. Perfect for session storage.

## Key Takeaways

- ETS is a built-in, in-memory key-value store with constant-time lookups
- Four table types: `set`, `ordered_set`, `bag`, `duplicate_bag`
- Concurrent reads with `{read_concurrency, true}`
- `update_counter` for atomic increments without locks
- Tables die with their owner — use a supervised process as owner
- Use ETS when you need speed and concurrency beyond what a GenServer provides
- `ets:fun2ms` makes match specifications human-readable

ETS is one of Erlang's not-so-secret weapons. It gives you the performance of a shared mutable data structure with the safety of the BEAM's concurrency model. When a GenServer map becomes a bottleneck, ETS is your escape hatch.

---

[← Previous: Supervision Trees](18-supervision-trees.md) | [Next: Mnesia →](20-mnesia.md)
