# Chapter 5: Atoms, Tuples, and Lists — Oh My

> *Erlang's type system is like a well-organized junk drawer. There aren't many types, but the ones that exist are exactly right, and you can build anything out of them. Also, strings are lists of integers. I'm sorry. I didn't make the rules.*

---

## The Type Landscape

Erlang has a small set of data types. Here they all are:

- **Numbers** — integers and floats
- **Atoms** — named constants
- **Tuples** — fixed-size collections
- **Lists** — variable-size collections
- **Binaries** — raw bytes
- **Maps** — key-value stores
- **Pids** — process identifiers
- **References** — unique values
- **Funs** — anonymous functions
- **Ports** — external program connections

That's it. No classes, no structs, no interfaces, no generics, no enums (atoms are better), no Optional types. You build everything from these primitives.

## Numbers

Integers in Erlang are arbitrary precision. They never overflow:

```erlang
1> 2 + 3.
5
2> 100 * 100 * 100 * 100 * 100 * 100 * 100.
100000000000000
3> factorial(100).  %% assuming you've written this function
93326215443944152681699238856266700490715968264381621468...
```

Floats are IEEE 754 doubles:

```erlang
4> 3.14 * 2.
6.28
5> 1 / 3.
0.3333333333333333
```

Fun notation tricks:

```erlang
6> 16#FF.         %% Hexadecimal
255
7> 2#1010.        %% Binary
10
8> $A.            %% Character code for 'A'
65
```

Integer arithmetic is exact. Float arithmetic has the usual IEEE surprises. Erlang won't implicitly convert between them — `1 + 1.0` works, but `1 =:= 1.0` is `false` (use `==` for loose comparison).

## Atoms

Atoms are Erlang's secret weapon. An atom is a literal — a constant whose name is its value:

```erlang
1> hello.
hello
2> world.
world
3> 'this is also an atom'.
'this is also an atom'
4> true.
true
5> false.
false
```

Yep, `true` and `false` are just atoms. There's no separate boolean type. The `and`, `or`, `not` operators just work on the atoms `true` and `false`.

Atoms are used for:

- **Tags**: `{ok, Value}`, `{error, Reason}`
- **Status codes**: `running`, `stopped`, `paused`
- **Options**: `[binary, {active, true}]`
- **Module names**: `lists`, `gen_server`
- **Function names internally**

```erlang
%% The classic Erlang result pattern
case file:read_file("config.txt") of
    {ok, Contents} -> parse(Contents);
    {error, enoent} -> default_config();
    {error, Reason} -> exit({config_error, Reason})
end.
```

**Warning**: Atoms are not garbage collected. They live forever in the atom table. Never create atoms dynamically from user input (`list_to_atom(UserInput)` is a denial-of-service vector). Use `list_to_existing_atom/1` if you must.

## Tuples

Tuples are fixed-size, ordered collections. They use curly braces:

```erlang
1> {ok, 42}.
{ok,42}
2> {person, "Alice", 30, "alice@example.com"}.
{person,"Alice",30,"alice@example.com"}
3> element(2, {a, b, c}).
b
4> tuple_size({x, y, z}).
3
```

Tuples are stored contiguously in memory, so accessing any element is O(1). The convention is to tag tuples with an atom as the first element:

```erlang
{point, 3, 4}
{rgb, 255, 128, 0}
{user, "Bob", "bob@example.com"}
```

This makes pattern matching clean:

```erlang
area({circle, Radius}) -> math:pi() * Radius * Radius;
area({rect, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side.
```

## Lists

Lists are linked lists (yes, linked lists — the data structure your CS professor said was important):

```erlang
1> [1, 2, 3, 4, 5].
[1,2,3,4,5]
2> [hello, "world", 42, {mixed, types}].
[hello,"world",42,{mixed,types}]
3> [] . %% Empty list
[]
```

The key operation on lists is **head/tail decomposition**:

```erlang
4> [Head | Tail] = [1, 2, 3, 4, 5].
[1,2,3,4,5]
5> Head.
1
6> Tail.
[2,3,4,5]
```

The `|` (cons) operator is how you build and decompose lists:

```erlang
7> [0 | [1, 2, 3]].
[0,1,2,3]
8> [1 | [2 | [3 | []]]].
[1,2,3]
```

That last one reveals the truth: `[1, 2, 3]` is syntactic sugar for `[1 | [2 | [3 | []]]]`. It's cons cells all the way down.

### List Comprehensions

Erlang has list comprehensions, and they're delightful:

```erlang
1> [X * 2 || X <- [1, 2, 3, 4, 5]].
[2,4,6,8,10]
2> [X || X <- lists:seq(1, 20), X rem 2 =:= 0].
[2,4,6,8,10,12,14,16,18,20]
3> [{X, Y} || X <- [1,2,3], Y <- [a,b]].
[{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}]
```

### Common List Operations

```erlang
1> lists:reverse([1, 2, 3]).
[3,2,1]
2> lists:sort([3, 1, 4, 1, 5, 9]).
[1,1,3,4,5,9]
3> lists:map(fun(X) -> X * X end, [1, 2, 3, 4]).
[1,4,9,16]
4> lists:filter(fun(X) -> X > 3 end, [1, 2, 3, 4, 5]).
[4,5]
5> lists:foldl(fun(X, Acc) -> X + Acc end, 0, [1, 2, 3, 4, 5]).
15
6> lists:flatten([[1, 2], [3, [4, 5]]]).
[1,2,3,4,5]
```

## The String Situation

Okay, deep breath. In Erlang, a string `"hello"` is a list of integers:

```erlang
1> "hello" =:= [104, 101, 108, 108, 111].
true
2> [72 | "ello"].
"Hello"
3> is_list("hello").
true
```

The shell is being "helpful" by displaying lists of printable integers as strings. This means:

```erlang
4> [97, 98, 99].
"abc"
```

Surprise! The shell decided that looked like a string. This confuses everyone at first. Welcome to the club.

For modern Erlang, use **binaries** for text:

```erlang
5> <<"hello">>.
<<"hello">>
6> is_binary(<<"hello">>).
true
7> <<H, Rest/binary>> = <<"hello">>.
<<"hello">>
8> H.
104
```

Binary strings are compact, efficient, and what you should use in production code.

## Binaries and Bit Syntax

Binaries are Erlang's way of handling raw bytes, and the bit syntax is ridiculously powerful:

```erlang
1> <<1, 2, 3>>.
<<1,2,3>>
2> <<"hello">>.
<<"hello">>
3> <<X:16/big>> = <<0, 255>>.
<<0,255>>
4> X.
255
```

You can parse binary protocols in a single pattern match:

```erlang
parse_ip_header(<<Version:4, IHL:4, DSCP:6, ECN:2,
                  TotalLength:16, _Rest/binary>>) ->
    {Version, IHL, DSCP, ECN, TotalLength}.
```

That just parsed the first fields of an IPv4 header in one line. Try doing that in Java.

## Maps

Maps are key-value stores, added in OTP 17:

```erlang
1> M = #{name => "Alice", age => 30}.
#{age => 30,name => "Alice"}
2> maps:get(name, M).
"Alice"
3> M#{age => 31}.
#{age => 31,name => "Alice"}
4> #{name := Name} = M.
#{age => 30,name => "Alice"}
5> Name.
"Alice"
```

Note the syntax: `=>` for creating/updating, `:=` for matching (matching requires the key to exist).

Maps are great for structured data that would be JSON in another language:

```erlang
User = #{
    id => 42,
    name => <<"Alice">>,
    email => <<"alice@example.com">>,
    roles => [admin, user]
}.
```

## Records (The "Before Maps" Solution)

Before maps existed, Erlang had records. They're still widely used:

```erlang
-record(user, {name, age, email}).

%% Create
U = #user{name = "Alice", age = 30, email = "alice@example.com"}.

%% Access
U#user.name.
%% "Alice"

%% Update
U2 = U#user{age = 31}.
```

Records are compile-time sugar over tuples. `#user{name="Alice", age=30}` becomes `{user, "Alice", 30, undefined}` at runtime. They're fast but brittle — adding a field requires recompiling everything.

## Pids, References, and Funs

```erlang
%% Pids - process identifiers
1> self().
<0.85.0>

%% References - globally unique values
2> make_ref().
#Ref<0.1234567890.1234567890.123456>

%% Funs - anonymous functions
3> Add = fun(A, B) -> A + B end.
#Fun<erl_eval.44.79398840>
4> Add(3, 4).
7
```

## Type Checking

Erlang has guard functions (BIFs) for checking types:

```erlang
is_integer(42).        %% true
is_float(3.14).        %% true
is_atom(hello).        %% true
is_list([1,2,3]).      %% true
is_tuple({a, b}).      %% true
is_binary(<<"hi">>).   %% true
is_map(#{a => 1}).     %% true
is_pid(self()).        %% true
is_function(fun() -> ok end). %% true
```

## Key Takeaways

- Erlang has a small, well-chosen set of types
- Atoms are named constants — use them everywhere
- Tag your tuples with atoms for clean pattern matching
- Lists are linked lists with `[Head | Tail]` decomposition
- Strings are lists of integers (use binaries instead)
- The bit syntax can parse binary protocols in a single expression
- Maps are for key-value data, records are the older alternative
- Everything is immutable — "updating" a map/record creates a new one

The type system is simple but you can build anything with these primitives. And because everything is immutable, you never have to worry about someone mutating your data behind your back.

---

[← Previous: Variables Don't Vary](04-variables.md) | [Next: Pattern Matching →](06-pattern-matching.md)
