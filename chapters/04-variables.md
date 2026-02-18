# Chapter 4: Variables Don't Vary (And That's Fine)

> *In every other language you've used, variables vary. That's literally what the word means. In Erlang, variables don't. They get bound once and that's it. If this sounds insane, congratulations — you're thinking like an imperative programmer. We're going to fix that.*

---

## The Big Reveal: `=` Is Not Assignment

This is the single most important thing to understand about Erlang syntax. The `=` operator is not assignment. It's **pattern matching**.

```erlang
1> X = 5.
5
```

This doesn't mean "put 5 into X." It means "match the left side against the right side." Since `X` is unbound, the only way to make the match succeed is to bind `X` to `5`. So it does.

Now watch what happens:

```erlang
2> X = 5.
5
3> X = 6.
** exception error: no match of right hand side value 6
```

`X = 5` succeeds because `X` is already `5` — the pattern matches. `X = 6` fails because `5 ≠ 6`. There's no "reassignment." There's only matching.

## Why? Because Concurrency.

This isn't arbitrary sadism. There's a reason.

When you have millions of concurrent processes, shared mutable state is the devil. If any process can change a variable at any time, you need locks, mutexes, semaphores — the whole cursed zoo of synchronization primitives.

Erlang sidesteps this entirely. If a value can't change, it can't be corrupted by another process. Problem solved. Not "mitigated." Not "handled with careful locking." *Solved.*

## Single Assignment in Practice

```erlang
1> Name = "Alice".
"Alice"
2> Age = 30.
30
3> {Name, Age}.
{"Alice",30}
```

Want a new value? Use a new variable:

```erlang
4> Name2 = "Bob".
"Bob"
```

In functions, this is natural because each clause gets fresh bindings:

```erlang
greet(Name) ->
    Greeting = "Hello, " ++ Name ++ "!",
    io:format("~s~n", [Greeting]).
```

Every time `greet/1` is called, `Name` and `Greeting` are fresh bindings. No state carries over between calls.

## The Underscore: The "I Don't Care" Variable

The underscore `_` is special. It matches anything and throws it away:

```erlang
1> {_, Second, _} = {1, 2, 3}.
{1,2,3}
2> Second.
2
```

Use it when you need to match a structure but don't care about certain parts. You can also use named underscores for documentation:

```erlang
{_Name, Age, _Email} = get_user(123).
```

Variables starting with underscore match like `_` but are named for readability. The compiler won't warn about them being unused.

## Pattern Matching with `=`

Since `=` is matching, you can do surprisingly powerful things with it:

```erlang
1> {ok, Value} = {ok, 42}.
{ok,42}
2> Value.
42
3> {ok, Value2} = {error, "something broke"}.
** exception error: no match of right hand side value
                    {error,"something broke"}
```

This is huge. You're simultaneously **destructuring** the tuple and **asserting** that the first element is `ok`. If the function returned an error, the match fails loudly. This is Erlang's primary error-handling mechanism — not try/catch, not null checks, just *matching*.

```erlang
%% This is an incredibly common Erlang pattern:
{ok, File} = file:open("data.txt", [read]).
%% If the file doesn't exist, this crashes immediately.
%% And that's exactly what we want.
```

## Variables in the Shell vs. in Code

In the shell, variables accumulate as you type expressions. Use `f()` to clear them:

```erlang
1> X = 1.
1
2> Y = 2.
2
3> f(X).
ok
4> X = 99.
99
```

In actual modules, this isn't an issue. Each function call starts with fresh bindings. The scope of a variable is the clause it appears in.

## Shadowing? Nope.

Unlike most functional languages, Erlang doesn't allow variable shadowing. If `X` is bound in a scope, you can't rebind it in a nested scope:

```erlang
%% This WON'T work:
example(X) ->
    Result = case X of
        1 -> X = "one";  %% ERROR: X is already bound
        2 -> "two"
    end.
```

This seems restrictive, but it prevents a whole class of bugs where you accidentally reuse a variable name and wonder why you're getting the wrong value.

## The Match Operator in Function Heads

You can pattern match directly in function parameters:

```erlang
handle_response({ok, Data}) ->
    process(Data);
handle_response({error, Reason}) ->
    log_error(Reason).
```

No if statements. No null checks. The right clause fires based on the shape of the data. This is one of Erlang's most elegant features, and we'll dedicate all of Chapter 6 to it.

## Strings Are Weird (A Preview)

A quick warning: strings in Erlang are lists of integers:

```erlang
1> "hello".
"hello"
2> "hello" =:= [104,101,108,108,111].
true
3> [72 | "ello"].
"Hello"
```

Yes, really. The string `"hello"` is syntactic sugar for a linked list of character codes. This is... a choice. Erlang has binary strings too (`<<"hello">>`) which are more efficient, and modern Erlang code tends to use them. We'll deal with this properly in the data types chapter.

## Key Takeaways

- `=` is pattern matching, not assignment
- Variables are single-assignment — once bound, they stay bound
- This eliminates entire categories of concurrency bugs
- `_` matches anything and discards it
- Pattern matching on `=` gives you destructuring and assertions in one operation
- There's no variable shadowing
- In functions, each call gets fresh bindings — this is natural and clean

The single-assignment model feels weird for about a day. Then it feels liberating. You never have to wonder "what value does this variable have right now?" because the answer is always "the one it was bound to." Period.

---

[← Previous: The Shell](03-the-shell.md) | [Next: Atoms, Tuples, and Lists →](05-data-types.md)
