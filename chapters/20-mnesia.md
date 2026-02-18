# Chapter 20: Mnesia: The Database That Lives in Your VM

> *Most databases live outside your application. You connect to them over a network, send queries in a different language, wait for results, and hope the connection doesn't drop. Mnesia lives inside the BEAM itself. It's a distributed, real-time, transactional database that stores Erlang terms directly. It can replicate across nodes automatically. It's completely bonkers, and sometimes it's exactly what you need.*

---

## What Is Mnesia?

Mnesia is a distributed database management system built into Erlang/OTP. It's:

- **Built into OTP** — No external server to install or manage
- **Distributed** — Replicates tables across BEAM nodes automatically
- **Transactional** — ACID transactions with built-in conflict resolution
- **Flexible storage** — RAM, disk, or both
- **Native Erlang** — No SQL, no ORM, just Erlang terms and pattern matching

It was built for the same telecom use case as everything else in Erlang — the systems that can't go down.

## When to Use Mnesia

**Good fit:**
- Configuration and metadata storage
- Session data shared across nodes
- Real-time lookup tables
- Small-to-medium datasets (millions of records)
- Data that needs to survive node restarts
- Distributed caches with consistency requirements

**Not a good fit:**
- Massive datasets (terabytes)
- Complex relational queries
- Data that needs to be shared with non-Erlang systems
- When you need a dedicated DBA team managing it

## Getting Started

```erlang
%% Create the schema (on disk)
mnesia:create_schema([node()]).

%% Start Mnesia
mnesia:start().

%% Create a table
mnesia:create_table(user, [
    {attributes, [id, name, email, created_at]},
    {disc_copies, [node()]}  %% Store on disk AND in RAM
]).

%% Wait for tables to be ready
mnesia:wait_for_tables([user], 5000).
```

### Storage Types

| Type | Where | Survives restart? | Speed |
|------|-------|-------------------|-------|
| `ram_copies` | RAM only | No | Fastest |
| `disc_copies` | RAM + disk | Yes | Fast reads, disk writes |
| `disc_only_copies` | Disk only | Yes | Slowest |

`disc_copies` gives you the best of both worlds — RAM-speed reads with persistent storage.

## CRUD Operations

### Writing

```erlang
%% Write a record inside a transaction
mnesia:transaction(fun() ->
    mnesia:write({user, 1, "Alice", "alice@example.com",
                  erlang:system_time(second)})
end).
%% {atomic, ok}
```

### Reading

```erlang
%% Read by key
mnesia:transaction(fun() ->
    mnesia:read({user, 1})
end).
%% {atomic, [{user, 1, "Alice", "alice@example.com", 1700000000}]}
```

### Deleting

```erlang
mnesia:transaction(fun() ->
    mnesia:delete({user, 1})
end).
%% {atomic, ok}
```

### Querying

```erlang
%% Find all users with a specific email domain
mnesia:transaction(fun() ->
    mnesia:select(user, [
        {{'user', '$1', '$2', '$3', '_'},
         [],  %% No guards
         ['$$']}  %% Return all match variables
    ])
end).

%% Using QLC (Query List Comprehensions) — much more readable
-include_lib("stdlib/include/qlc.hrl").

find_users_by_domain(Domain) ->
    mnesia:transaction(fun() ->
        Q = qlc:q([{Name, Email} ||
            {user, _Id, Name, Email, _Created} <- mnesia:table(user),
            string:find(Email, Domain) =/= nomatch
        ]),
        qlc:eval(Q)
    end).
```

## Transactions

Mnesia transactions are ACID-compliant:

```erlang
%% Transfer credits between users
transfer(FromId, ToId, Amount) ->
    mnesia:transaction(fun() ->
        [From] = mnesia:read({account, FromId}),
        [To] = mnesia:read({account, ToId}),
        {account, _, FromBalance} = From,
        {account, _, ToBalance} = To,
        true = FromBalance >= Amount,  %% Assert sufficient funds
        mnesia:write({account, FromId, FromBalance - Amount}),
        mnesia:write({account, ToId, ToBalance + Amount})
    end).
```

If anything in the transaction fails (pattern match, assertion, crash), the entire transaction is rolled back. No partial updates.

### Dirty Operations

For performance-critical reads where you don't need transactional guarantees:

```erlang
%% Much faster, but no transaction guarantees
mnesia:dirty_read({user, 1}).
mnesia:dirty_write({user, 2, "Bob", "bob@example.com", 0}).
mnesia:dirty_delete({user, 3}).
```

Dirty operations bypass the transaction manager and go straight to the data. Use them for read-heavy workloads where eventual consistency is acceptable.

## Table Types

Mnesia supports different table structures:

```erlang
%% Set (default): unique keys
mnesia:create_table(user, [
    {attributes, [id, name, email]},
    {type, set}
]).

%% Ordered set: unique keys, sorted
mnesia:create_table(event, [
    {attributes, [timestamp, type, data]},
    {type, ordered_set}
]).

%% Bag: multiple records per key
mnesia:create_table(user_role, [
    {attributes, [user_id, role]},
    {type, bag}
]).
```

## Using Records with Mnesia

Records make Mnesia much more pleasant:

```erlang
-record(user, {id, name, email, created_at}).

setup() ->
    mnesia:create_table(user, [
        {attributes, record_info(fields, user)},
        {disc_copies, [node()]}
    ]).

add_user(Id, Name, Email) ->
    mnesia:transaction(fun() ->
        mnesia:write(#user{id = Id,
                          name = Name,
                          email = Email,
                          created_at = erlang:system_time(second)})
    end).

get_user(Id) ->
    mnesia:transaction(fun() ->
        case mnesia:read({user, Id}) of
            [User] -> {ok, User};
            [] -> {error, not_found}
        end
    end).
```

## Distributed Mnesia

Mnesia can replicate across nodes. This is where it gets powerful:

```erlang
%% On node1: create schema for both nodes
mnesia:create_schema([node1@host, node2@host]).

%% Start mnesia on both nodes
%% On node1:
mnesia:start().
mnesia:create_table(user, [
    {attributes, record_info(fields, user)},
    {disc_copies, [node1@host, node2@host]}  %% Replicate to both
]).

%% On node2:
mnesia:start().
mnesia:wait_for_tables([user], 5000).
```

Now both nodes have a copy of the user table. Writes are replicated automatically. Reads are local (fast). This gives you:

- **High availability** — If one node goes down, the other still has the data
- **Read scaling** — Each node reads from its local copy
- **Automatic sync** — Nodes catch up when they reconnect

## Mnesia Gotchas

### Table fragmentation for large tables

Single tables are limited by memory. For very large tables, use fragmentation:

```erlang
mnesia:create_table(big_table, [
    {attributes, [key, value]},
    {frag_properties, [{n_fragments, 8}, {n_disc_copies, 1}]}
]).
```

### Network splits

Mnesia doesn't handle network partitions (split brain) automatically. When nodes can't talk, tables become inconsistent. You need to configure a conflict resolution strategy:

```erlang
%% Set the master node for conflict resolution
mnesia:set_master_nodes(user, [node1@host]).
```

### Schema changes

Adding or removing columns is possible but requires careful migration:

```erlang
mnesia:transform_table(user,
    fun({user, Id, Name, Email}) ->
        {user, Id, Name, Email, undefined}  %% Add created_at field
    end,
    [id, name, email, created_at]).
```

## Practical Example: A Configuration Store

```erlang
-module(config_store).
-export([init/0, set/2, get/1, get/2, all/0]).

-record(config, {key, value}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(config, [
        {attributes, record_info(fields, config)},
        {disc_copies, [node()]}
    ]),
    mnesia:wait_for_tables([config], 5000).

set(Key, Value) ->
    mnesia:transaction(fun() ->
        mnesia:write(#config{key = Key, value = Value})
    end).

get(Key) ->
    get(Key, undefined).

get(Key, Default) ->
    case mnesia:dirty_read({config, Key}) of
        [#config{value = Value}] -> Value;
        [] -> Default
    end.

all() ->
    mnesia:dirty_match_object(#config{_ = '_'}).
```

Persistent, survives restarts, fast reads via dirty operations, and consistent writes via transactions.

## Key Takeaways

- Mnesia is a distributed, transactional database built into the BEAM
- It stores Erlang terms directly — no ORM, no serialization
- Three storage modes: RAM, disk, or both
- Transactions provide ACID guarantees
- Dirty operations are faster but skip transactional safety
- Tables can be replicated across nodes automatically
- Use records for cleaner Mnesia code
- Mnesia struggles with network partitions — plan your strategy
- Great for config, sessions, and metadata; not for massive datasets

Mnesia is one of Erlang's quirkiest features. It's not a replacement for PostgreSQL, and it won't scale to petabytes. But for the sweet spot of small-to-medium datasets that need to be fast, distributed, and tightly integrated with your Erlang code, nothing else comes close.

---

[← Previous: ETS](19-ets.md) | [Next: Hot Code Reloading →](21-hot-code-reloading.md)
