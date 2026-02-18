# Chapter 8: Recursion: Who Needs Loops?

> *Erlang has no for loops. No while loops. No do-while. No foreach. No loop keyword at all. When people hear this, they usually panic. But here's the secret: you don't need loops. You never needed loops. Loops were a lie told to us by mutable state. Recursion is the truth. And in Erlang, it's fast, natural, and beautiful.*

---

## Why No Loops?

Loops require mutation. Think about it:

```python
# Python
total = 0
for x in [1, 2, 3, 4, 5]:
    total = total + x  # <-- mutation!
```

You're repeatedly changing `total`. In a language with single assignment, this is impossible. So instead:

```erlang
sum([]) -> 0;
sum([H | T]) -> H + sum(T).
```

```erlang
1> sum([1, 2, 3, 4, 5]).
15
```

Two clauses. No mutation. No loop variable. Just a function that calls itself until the list is empty.

## How It Works

Let's trace `sum([1, 2, 3])`:

```
sum([1, 2, 3])
= 1 + sum([2, 3])
= 1 + (2 + sum([3]))
= 1 + (2 + (3 + sum([])))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
```

Each call creates a new stack frame, does its work, then returns. The base case (`sum([]) -> 0`) stops the recursion.

## Tail Recursion: The Fast Kind

The `sum` above has a problem — it builds up stack frames. For a million-element list, that's a million frames. Let's fix that:

```erlang
sum(List) -> sum(List, 0).

sum([], Acc) -> Acc;
sum([H | T], Acc) -> sum(T, Acc + H).
```

Trace `sum([1, 2, 3], 0)`:

```
sum([1, 2, 3], 0)
sum([2, 3], 1)
sum([3], 3)
sum([], 6)
6
```

No stack buildup! The recursive call is the *last thing* the function does (a "tail call"), so the BEAM reuses the current stack frame. This runs in constant stack space, just like a loop.

**The pattern**: use an accumulator parameter to carry state forward. The base case returns the accumulator.

## Classic Recursive Patterns

### Length of a list

```erlang
len([]) -> 0;
len([_ | T]) -> 1 + len(T).

%% Tail recursive version
len(List) -> len(List, 0).
len([], Acc) -> Acc;
len([_ | T], Acc) -> len(T, Acc + 1).
```

### Reverse a list

```erlang
reverse(List) -> reverse(List, []).
reverse([], Acc) -> Acc;
reverse([H | T], Acc) -> reverse(T, [H | Acc]).
```

This is O(n) and tail-recursive. The trick: prepend each head to the accumulator.

### Map (apply a function to each element)

```erlang
map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].
```

```erlang
1> map(fun(X) -> X * 2 end, [1, 2, 3]).
[2,4,6]
```

### Filter

```erlang
filter(_, []) -> [];
filter(Pred, [H | T]) ->
    case Pred(H) of
        true -> [H | filter(Pred, T)];
        false -> filter(Pred, T)
    end.
```

### Fibonacci (The Classic)

```erlang
%% Naive (exponential time — don't do this in production)
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

%% Linear time with accumulator
fib_fast(N) -> fib_fast(N, 0, 1).
fib_fast(0, A, _) -> A;
fib_fast(N, A, B) -> fib_fast(N - 1, B, A + B).
```

```erlang
1> fib_fast(50).
12586269025
```

The naive version would still be computing `fib(50)` when the heat death of the universe arrives. The accumulator version does it instantly.

## Recursion Beyond Lists

Recursion works on anything, not just lists:

### Countdown

```erlang
countdown(0) ->
    io:format("Blast off!~n");
countdown(N) when N > 0 ->
    io:format("~p...~n", [N]),
    countdown(N - 1).
```

```erlang
1> countdown(5).
5...
4...
3...
2...
1...
Blast off!
ok
```

### Greatest Common Divisor (Euclid's Algorithm)

```erlang
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).
```

```erlang
1> gcd(48, 18).
6
```

Three lines. 2,300 years of mathematical history. Pattern matching makes it trivially clear.

### Tree Traversal

```erlang
%% A binary tree: {node, Value, Left, Right} or leaf
sum_tree(leaf) -> 0;
sum_tree({node, Value, Left, Right}) ->
    Value + sum_tree(Left) + sum_tree(Right).
```

```erlang
1> Tree = {node, 1,
1>     {node, 2, leaf, leaf},
1>     {node, 3, leaf, {node, 4, leaf, leaf}}}.
2> sum_tree(Tree).
10
```

## Infinite Loops (On Purpose)

In Erlang, many processes are *supposed* to loop forever. Here's the pattern:

```erlang
-module(echo_server).
-export([start/0, loop/0]).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
        {echo, From, Msg} ->
            From ! {reply, Msg},
            loop();  %% <-- tail-recursive loop!
        stop ->
            ok  %% exit the loop (process terminates)
    end.
```

```erlang
1> Pid = echo_server:start().
<0.89.0>
2> Pid ! {echo, self(), "hello"}.
{echo,<0.85.0>,"hello"}
3> flush().
Shell got {reply,"hello"}
ok
```

The `loop()` function calls itself after handling each message. Because it's a tail call, it runs forever without consuming stack space. This is how every Erlang process works — an infinite tail-recursive loop.

## When to Use Tail Recursion

**Always?** Not necessarily. The BEAM is good at handling both styles:

- **Tail recursive**: Use when processing large lists or implementing process loops
- **Non-tail recursive**: Fine for small inputs or when the code is cleaner

The `lists` module itself uses a mix of both. Don't contort your code into tail-recursive form if it makes it unreadable. The `[H | map(F, T)]` pattern is idiomatic and fine for typical list sizes.

That said, process loops must always be tail-recursive. A process that builds up stack will eventually run out of memory.

## Key Takeaways

- Erlang has no loops — recursion handles everything
- Base case + recursive case is the fundamental structure
- Tail recursion (recursive call as the last operation) runs in constant stack space
- Use accumulators to make recursion tail-recursive
- Process loops are just tail-recursive functions that call `receive`
- Don't over-optimize — non-tail-recursive is fine for small inputs
- Pattern matching makes recursive functions remarkably concise

Recursion in Erlang isn't a workaround for missing loops. It's a *better* tool. Once you've written a few recursive functions, you'll wonder why you ever wanted loops in the first place.

---

[← Previous: Functions and Modules](07-functions-and-modules.md) | [Next: The BEAM →](09-the-beam.md)
