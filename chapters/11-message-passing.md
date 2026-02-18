# Chapter 11: Message Passing: No Shared Memory, No Problems

> *In most languages, concurrent threads communicate by sharing memory and protecting it with locks. This is like sharing a bathroom with ten roommates and coordinating access with a system of colored Post-it notes. It technically works, but someone's going to have a bad time. Erlang takes a different approach: every process gets its own bathroom. If you need to tell someone something, you slide a note under their door.*

---

## The Send Operator: `!`

Sending a message is the simplest thing in Erlang:

```erlang
Pid ! Message
```

That's it. `!` (called "bang") sends `Message` to the process identified by `Pid`. The message can be *anything* — an atom, a tuple, a list, a map, a fun, another PID.

```erlang
1> self() ! hello.
hello
2> self() ! {data, [1, 2, 3]}.
{data,[1,2,3]}
3> self() ! #{type => greeting, text => <<"hi">>}.
#{text => <<"hi">>,type => greeting}
```

We just sent three messages to ourselves. They're sitting in our mailbox. Let's read them.

## The Receive Expression

```erlang
4> receive Msg -> Msg end.
hello
5> receive Msg2 -> Msg2 end.
{data,[1,2,3]}
6> receive Msg3 -> Msg3 end.
#{text => <<"hi">>,type => greeting}
```

`receive` blocks until a message arrives that matches one of its patterns. Messages are processed in mailbox order (FIFO), and matching is done top-to-bottom across the clauses.

## Pattern Matching in Receive

This is where it gets powerful. You can selectively receive messages:

```erlang
receive
    {ping, From} ->
        From ! pong;
    {data, Payload} ->
        process_data(Payload);
    stop ->
        ok
end
```

The process will only handle messages matching these patterns. Non-matching messages stay in the mailbox for later.

## A Complete Example: Ping Pong

```erlang
-module(pingpong).
-export([start/0]).

start() ->
    Pong = spawn(fun pong/0),
    Ping = spawn(fun() -> ping(Pong, 3) end),
    {Ping, Pong}.

ping(Pong, 0) ->
    Pong ! stop,
    io:format("Ping: done~n");
ping(Pong, N) ->
    Pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping: got pong (~p left)~n", [N - 1]),
            ping(Pong, N - 1)
    end.

pong() ->
    receive
        {ping, From} ->
            io:format("Pong: got ping, sending pong~n"),
            From ! pong,
            pong();
        stop ->
            io:format("Pong: stopping~n")
    end.
```

```erlang
1> pingpong:start().
Pong: got ping, sending pong
Ping: got pong (2 left)
Pong: got ping, sending pong
Ping: got pong (1 left)
Pong: got ping, sending pong
Ping: got pong (0 left)
Pong: stopping
Ping: done
```

Two processes, communicating exclusively through messages. No shared state, no locks, no race conditions.

## Timeouts

What if a message never comes? Use `after`:

```erlang
receive
    {response, Data} ->
        {ok, Data}
after 5000 ->
    {error, timeout}
end
```

After 5000 milliseconds with no matching message, the `after` clause fires. This is essential for building robust systems.

```erlang
%% Wait for any message with a timeout
wait_for_anything() ->
    receive
        Msg ->
            io:format("Got: ~p~n", [Msg])
    after 1000 ->
        io:format("Nothing after 1 second~n")
    end.
```

The special timeout `0` means "check the mailbox but don't wait":

```erlang
%% Non-blocking receive
flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.
```

This drains all messages from the mailbox without blocking. Handy for cleanup.

## Message Ordering Guarantees

Erlang guarantees that messages from process A to process B arrive **in the order they were sent**. But messages from different senders have no ordering guarantee relative to each other.

```erlang
%% Process A sends to C
A -> C: msg1, msg2, msg3  %% C receives in this order ✓

%% But if A and B both send to C:
A -> C: msg_a1, msg_a2
B -> C: msg_b1, msg_b2
%% C might receive: msg_a1, msg_b1, msg_a2, msg_b2
%% or: msg_b1, msg_a1, msg_b2, msg_a2
%% or any interleaving — BUT msg_a1 always before msg_a2,
%% and msg_b1 always before msg_b2
```

## Selective Receive

The receive clause patterns let you selectively process messages out of order:

```erlang
%% Only process "urgent" messages first
receive
    {urgent, Msg} -> handle_urgent(Msg)
after 0 ->
    receive
        {normal, Msg} -> handle_normal(Msg)
    end
end
```

Be careful with selective receive, though. If your mailbox fills up with messages that don't match any pattern, the process has to scan through all of them on every `receive`. This is the **mailbox scanning problem** — a common performance pitfall.

## The Request-Reply Pattern

The most common messaging pattern in Erlang:

```erlang
%% Client side
request(Server, Request) ->
    Ref = make_ref(),
    Server ! {request, self(), Ref, Request},
    receive
        {reply, Ref, Response} ->
            Response
    after 5000 ->
        {error, timeout}
    end.

%% Server side (in the receive loop)
loop(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NewState} = handle(Request, State),
            From ! {reply, Ref, Response},
            loop(NewState)
    end.
```

The `make_ref()` creates a unique reference. By including it in the request and matching on it in the reply, we ensure we get the response to *our* specific request, not someone else's.

This pattern is so common that OTP wraps it into `gen_server:call/2` (Chapter 16).

## Messages Are Copied

When you send a message, the data is **copied** from the sender's heap to the receiver's heap. This is essential for process isolation — if processes shared data, a GC in one process could invalidate pointers in another.

The copying has costs:
- Sending large messages is expensive
- Each process needs enough heap space for its incoming messages

But the benefits are enormous:
- True isolation — no shared-state bugs
- Per-process GC — no global pauses
- Distribution — copying to a remote node is the same operation, just over a network

Large binaries (>64 bytes) are an exception — they live on a shared reference-counted heap and only a reference is copied. This is why binaries are preferred for large data.

## Building a Chat Room

Let's put it all together with a simple chat room:

```erlang
-module(chat).
-export([start/0, join/2, say/2]).

start() ->
    spawn(fun() -> room([]) end).

join(Room, Name) ->
    Room ! {join, self(), Name},
    receive
        {welcome, Members} -> {ok, Members}
    after 5000 -> {error, timeout}
    end.

say(Room, Text) ->
    Room ! {say, self(), Text}.

room(Members) ->
    receive
        {join, Pid, Name} ->
            io:format("~s joined~n", [Name]),
            Pid ! {welcome, [N || {_, N} <- Members]},
            broadcast(Members, {joined, Name}),
            room([{Pid, Name} | Members]);
        {say, Pid, Text} ->
            case lists:keyfind(Pid, 1, Members) of
                {Pid, Name} ->
                    broadcast(Members, {message, Name, Text});
                false ->
                    ok
            end,
            room(Members)
    end.

broadcast(Members, Msg) ->
    [Pid ! Msg || {Pid, _} <- Members].
```

A chat room in 30 lines. The room process holds the member list as state. Members communicate through it by sending messages. No database, no websocket library, no framework — just processes and messages.

## Key Takeaways

- `!` sends a message, `receive` reads one
- Messages can be any Erlang term
- Pattern matching in `receive` enables selective processing
- `after` provides timeouts
- Messages between two specific processes are ordered (FIFO)
- Messages are copied between process heaps (except large binaries)
- The request-reply pattern with `make_ref()` is fundamental
- Selective receive is powerful but watch for mailbox growth

Message passing is the heart of Erlang concurrency. No shared memory means no data races. No locks means no deadlocks. The programming model is simple: send a message, wait for a reply. Everything else is built on this foundation.

---

[← Previous: Processes](10-processes.md) | [Next: Let It Crash →](12-let-it-crash.md)
