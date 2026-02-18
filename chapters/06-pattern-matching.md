# Chapter 6: Pattern Matching: The Superpower

> *If you could only learn one thing about Erlang, it should be pattern matching. It replaces if/else chains, switch statements, null checks, type assertions, destructuring assignments, and about 40% of the code you'd write in any other language. It's the single feature that makes Erlang programmers insufferably smug, and honestly, they've earned it.*

---

## What Is Pattern Matching?

Pattern matching is a way of checking a value against a pattern and extracting parts of it at the same time. You've already seen it with `=`:

```erlang
1> {ok, Value} = {ok, 42}.
{ok,42}
2> Value.
42
```

But this is just the beginning. Pattern matching happens *everywhere* in Erlang:

- In variable binding (`=`)
- In function heads
- In `case` expressions
- In `receive` blocks
- In list comprehensions
- In binary comprehensions
- In map matching
- In try/catch

It's the primary control flow mechanism. Where other languages use `if`, Erlang matches.

## Matching Tuples

```erlang
%% Extract fields from a tagged tuple
{point, X, Y} = {point, 3, 4}.
%% X = 3, Y = 4

%% Assert and extract
{ok, Result} = some_function().
%% Crashes if some_function() doesn't return {ok, _}

%% Nested matching
{user, {name, First, Last}, Age} = {user, {name, "Joe", "Armstrong"}, 68}.
%% First = "Joe", Last = "Armstrong", Age = 68
```

## Matching Lists

```erlang
%% Head and tail
[First | Rest] = [1, 2, 3, 4, 5].
%% First = 1, Rest = [2, 3, 4, 5]

%% First two elements
[A, B | _] = [10, 20, 30, 40].
%% A = 10, B = 20

%% Exact match
[1, 2, 3] = [1, 2, 3].
%% ok

%% Fails
[1, 2, 3] = [1, 2, 4].
%% ** exception error: no match
```

## Matching in Function Heads

This is where pattern matching really shines. Instead of a big function with conditional logic inside, you write multiple function clauses:

```erlang
-module(shapes).
-export([area/1, describe/1]).

area({circle, Radius}) ->
    math:pi() * Radius * Radius;
area({rectangle, Width, Height}) ->
    Width * Height;
area({triangle, Base, Height}) ->
    0.5 * Base * Height.

describe({circle, _}) -> "A round thing";
describe({rectangle, W, H}) when W =:= H -> "A square, technically";
describe({rectangle, _, _}) -> "A rectangle";
describe({triangle, _, _}) -> "A triangle".
```

```erlang
1> shapes:area({circle, 5}).
78.53981633974483
2> shapes:area({rectangle, 3, 4}).
12
3> shapes:describe({rectangle, 5, 5}).
"A square, technically"
```

Each clause is tried in order, top to bottom. The first one that matches wins. This is clean, readable, and leaves no room for forgotten cases.

## Guards: When Patterns Aren't Enough

Sometimes you need to check a condition beyond structural matching. That's what guards are for:

```erlang
abs_value(X) when X < 0 -> -X;
abs_value(X) -> X.

classify_age(Age) when Age < 13 -> child;
classify_age(Age) when Age < 18 -> teenager;
classify_age(Age) when Age < 65 -> adult;
classify_age(_) -> senior.

head([H | _]) when is_integer(H), H > 0 -> {positive, H};
head([H | _]) when is_integer(H) -> {non_positive, H};
head([H | _]) -> {not_integer, H};
head([]) -> empty.
```

Guards use `,` for "and" and `;` for "or". Only a limited set of functions are allowed in guards (no side effects — you can't call `io:format` in a guard):

```erlang
%% Allowed in guards:
%% - Comparisons: ==, /=, =:=, =/=, <, >, =<, >=
%% - Boolean: and, or, not, andalso, orelse
%% - Arithmetic: +, -, *, div, rem
%% - Type checks: is_atom/1, is_integer/1, is_list/1, etc.
%% - BIFs: abs/1, length/1, tuple_size/1, map_size/1, etc.
```

## Case Expressions

`case` is pattern matching as an expression:

```erlang
describe_number(X) ->
    case X rem 2 of
        0 -> "even";
        1 -> "odd"
    end.

handle_result(Result) ->
    case Result of
        {ok, Value} ->
            io:format("Success: ~p~n", [Value]),
            Value;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            error;
        Other ->
            io:format("Unexpected: ~p~n", [Other]),
            {unexpected, Other}
    end.
```

You can use guards in case expressions too:

```erlang
categorize(X) ->
    case X of
        N when is_integer(N), N > 0 -> positive_integer;
        N when is_integer(N), N < 0 -> negative_integer;
        0 -> zero;
        N when is_float(N) -> a_float;
        _ -> something_else
    end.
```

## Matching Maps

```erlang
%% Extract specific keys
#{name := Name, age := Age} = #{name => "Alice", age => 30, email => "a@b.com"}.
%% Name = "Alice", Age = 30
%% (email is ignored — you don't have to match everything)

%% In function heads
greet(#{name := Name, title := Title}) ->
    io:format("Hello, ~s ~s!~n", [Title, Name]);
greet(#{name := Name}) ->
    io:format("Hey, ~s!~n", [Name]).
```

Map matching is *partial* — you only need to specify the keys you care about. This is different from tuple matching, which must match the entire structure.

## Matching Binaries

The binary pattern matching is where Erlang shows off:

```erlang
%% Parse a 32-bit big-endian integer
<<Value:32/big>> = <<0, 0, 1, 0>>.
%% Value = 256

%% Split a binary
<<First:3/binary, Rest/binary>> = <<"hello world">>.
%% First = <<"hel">>, Rest = <<"lo world">>

%% Parse an RGB color
<<R:8, G:8, B:8>> = <<255, 128, 0>>.
%% R = 255, G = 128, B = 0
```

Real-world example — parsing a simple binary protocol:

```erlang
parse_packet(<<Type:8, Length:16, Payload:Length/binary, _Rest/binary>>) ->
    {Type, Payload}.
```

Note how `Length` (extracted from the header) is used *in the same pattern* to determine how many bytes to grab for `Payload`. This is insanely powerful.

## The Power of Clause Ordering

Function clauses are tried top-to-bottom. Use this for specificity:

```erlang
%% Specific cases first, general fallback last
handle(ping) -> pong;
handle({echo, Msg}) -> Msg;
handle({add, A, B}) -> A + B;
handle(Unknown) -> {error, {unknown_command, Unknown}}.
```

If you put the general case first, the specific ones would never be reached. The compiler will actually warn you about unreachable clauses — pay attention to those warnings.

## Combining Patterns

You can match the same value from multiple angles using `=` within patterns:

```erlang
%% Bind the whole tuple AND extract the name
User = {user, Name, _Age} = {user, "Alice", 30}.
%% User = {user, "Alice", 30}, Name = "Alice"
```

This is handy when you need both the whole structure and parts of it:

```erlang
process_message({msg, _From, _To, _Body} = Message) ->
    log(Message),          %% log the whole thing
    {msg, From, _, _} = Message,
    route_to(From).        %% but also use parts of it
```

## A Real-World Example

Here's a simple calculator that demonstrates pattern matching in action:

```erlang
-module(calc).
-export([eval/1]).

eval({num, N}) -> N;
eval({add, A, B}) -> eval(A) + eval(B);
eval({mul, A, B}) -> eval(A) * eval(B);
eval({neg, A}) -> -eval(A).
```

```erlang
1> calc:eval({add, {num, 1}, {mul, {num, 2}, {num, 3}}}).
7
```

That's a tree-walking interpreter in four lines. No visitor pattern, no switch statements, no type casting. Just pattern matching.

## Key Takeaways

- Pattern matching is Erlang's primary control flow mechanism
- It happens in `=` bindings, function heads, `case`, `receive`, and more
- Function clauses replace if/else chains with clear, separated logic
- Guards add extra conditions beyond structural matching
- Binary pattern matching can parse protocols in a single expression
- Clause ordering matters — specific patterns first, general fallbacks last
- Pattern matching simultaneously asserts structure and extracts data

This is the feature that, once you get it, makes you annoyed at every language that doesn't have it. You'll start writing Python and think "why can't I just match on this?" Welcome to the club.

---

[← Previous: Data Types](05-data-types.md) | [Next: Functions and Modules →](07-functions-and-modules.md)
