# Chapter 7: Functions and Modules: Where Code Lives

> *Up until now, you've been typing things in the shell like a barbarian. It's time to write actual files. Erlang modules are simple, single-purpose units of code — no classes, no inheritance, no AbstractFactoryBeanProviderStrategyAdapter. Just functions in a file.*

---

## Anatomy of a Module

Every Erlang module lives in a `.erl` file. Here's the simplest possible module:

```erlang
-module(greeter).
-export([hello/1]).

hello(Name) ->
    io:format("Hello, ~s!~n", [Name]).
```

That's it. Let's break it down:

- `-module(greeter).` — This module is called `greeter`. The filename must be `greeter.erl`. Not negotiable.
- `-export([hello/1]).` — The function `hello` with arity 1 (one argument) is public. Everything else is private.
- `hello(Name) ->` — Function definition. `Name` is a parameter (starts with uppercase because it's a variable).
- `io:format("Hello, ~s!~n", [Name]).` — The function body. `~s` is a string format specifier, `~n` is a newline.

Compile and run:

```erlang
1> c(greeter).
{ok,greeter}
2> greeter:hello("world").
Hello, world!
ok
```

## Functions Have Arity

In Erlang, a function is identified by its name *and* its arity (number of arguments). `foo/1` and `foo/2` are completely different functions:

```erlang
-module(math_stuff).
-export([add/2, add/3]).

add(A, B) -> A + B.
add(A, B, C) -> A + B + C.
```

```erlang
1> math_stuff:add(1, 2).
3
2> math_stuff:add(1, 2, 3).
6
```

This is not overloading in the Java sense. These are separate functions that happen to share a name. The `/2` and `/3` notation is how you refer to them.

## Multiple Clauses

A function can have multiple clauses, separated by semicolons. The clauses are tried in order:

```erlang
-module(animals).
-export([sound/1]).

sound(dog) -> "woof";
sound(cat) -> "meow";
sound(cow) -> "moo";
sound(fox) -> "nobody knows";
sound(_) -> "...".
```

```erlang
1> animals:sound(dog).
"woof"
2> animals:sound(fox).
"nobody knows"
3> animals:sound(platypus).
"..."
```

Note the punctuation:
- **Semicolons** (`;`) separate clauses of the same function
- **Periods** (`.`) end the function definition
- **Commas** (`,`) separate expressions within a clause body

This punctuation will trip you up for the first week. Then it becomes second nature.

## Private Functions

Any function not in the `-export` list is private to the module:

```erlang
-module(secret).
-export([public_api/1]).

public_api(X) ->
    Result = hidden_helper(X),
    {ok, Result}.

hidden_helper(X) ->
    X * X + 1.
```

```erlang
1> secret:public_api(5).
{ok,26}
2> secret:hidden_helper(5).
** exception error: undefined function secret:hidden_helper/1
```

This is Erlang's encapsulation. No `private` keyword, no access modifiers — just the export list.

## Anonymous Functions (Funs)

Anonymous functions use the `fun` keyword:

```erlang
1> Double = fun(X) -> X * 2 end.
#Fun<erl_eval.44.79398840>
2> Double(21).
42
3> lists:map(fun(X) -> X * X end, [1, 2, 3, 4]).
[1,4,9,16]
```

Funs can have multiple clauses too:

```erlang
1> Classify = fun
1>     (X) when X > 0 -> positive;
1>     (0) -> zero;
1>     (_) -> negative
1> end.
2> Classify(-5).
negative
```

You can capture a named function as a fun:

```erlang
1> F = fun lists:reverse/1.
#Fun<lists.reverse.1>
2> F([1, 2, 3]).
[3,2,1]
```

## Higher-Order Functions

Functions that take or return other functions. The `lists` module is full of them:

```erlang
%% Map: apply a function to every element
1> lists:map(fun(X) -> X + 1 end, [1, 2, 3]).
[2,3,4]

%% Filter: keep elements that satisfy a predicate
2> lists:filter(fun(X) -> X > 3 end, [1, 2, 3, 4, 5]).
[4,5]

%% Fold: reduce a list to a single value
3> lists:foldl(fun(X, Acc) -> X + Acc end, 0, [1, 2, 3, 4, 5]).
15

%% Any/All
4> lists:any(fun(X) -> X > 4 end, [1, 2, 3, 4, 5]).
true
5> lists:all(fun(X) -> X > 4 end, [1, 2, 3, 4, 5]).
false
```

Write your own:

```erlang
-module(hof).
-export([apply_twice/2]).

apply_twice(F, X) ->
    F(F(X)).
```

```erlang
1> hof:apply_twice(fun(X) -> X * 2 end, 3).
12
```

## Module Attributes

Modules can have attributes beyond `-module` and `-export`:

```erlang
-module(my_app).
-author("Alice").
-vsn("1.0.0").
-export([start/0]).

-define(MAX_RETRIES, 3).
-define(TIMEOUT, 5000).

start() ->
    io:format("Starting with max retries: ~p~n", [?MAX_RETRIES]).
```

Common attributes:
- `-export([...]).` — Public functions
- `-import(Module, [Fun/Arity]).` — Import functions (rarely used — explicit `Module:Fun` is preferred)
- `-define(Name, Value).` — Macros (use `?Name` to reference)
- `-include("header.hrl").` — Include header files
- `-record(name, {fields}).` — Record definitions
- `-behaviour(gen_server).` — Declare OTP behaviour
- `-spec` and `-type` — Type specifications (for Dialyzer)

## Type Specs (Optional but Smart)

Erlang is dynamically typed, but you can add type annotations for documentation and static analysis:

```erlang
-module(math_utils).
-export([safe_div/2]).

-spec safe_div(number(), number()) -> {ok, float()} | {error, division_by_zero}.
safe_div(_, 0) -> {error, division_by_zero};
safe_div(A, B) -> {ok, A / B}.
```

These specs are checked by Dialyzer, Erlang's static analysis tool. They don't affect runtime behavior but catch type errors before they happen.

## Compiling Modules

From the shell:

```erlang
1> c(my_module).  %% Compiles my_module.erl, loads it
{ok,my_module}
```

From the command line:

```bash
erlc my_module.erl          # Produces my_module.beam
erl -noshell -s my_module start -s init stop
```

The `.beam` file is the compiled bytecode that runs on the BEAM VM. It's portable across platforms — compile once, run anywhere the BEAM runs.

## A Complete Example

Let's build a simple temperature converter:

```erlang
-module(temp).
-export([convert/2, freezing/1, boiling/1]).

-spec convert(number(), {atom(), atom()}) -> number().
convert(Temp, {celsius, fahrenheit}) ->
    Temp * 9 / 5 + 32;
convert(Temp, {fahrenheit, celsius}) ->
    (Temp - 32) * 5 / 9;
convert(Temp, {celsius, kelvin}) ->
    Temp + 273.15;
convert(Temp, {kelvin, celsius}) ->
    Temp - 273.15;
convert(Temp, {Same, Same}) ->
    Temp.

freezing(celsius)    -> 0;
freezing(fahrenheit) -> 32;
freezing(kelvin)     -> 273.15.

boiling(celsius)    -> 100;
boiling(fahrenheit) -> 212;
boiling(kelvin)     -> 373.15.
```

```erlang
1> temp:convert(100, {celsius, fahrenheit}).
212.0
2> temp:convert(72, {fahrenheit, celsius}).
22.22222222222222
3> temp:convert(42, {celsius, celsius}).
42
4> temp:freezing(kelvin).
273.15
```

Notice the `{Same, Same}` pattern — it matches any tuple where both elements are the same atom. Pattern matching handles the identity conversion without special-casing.

## Key Takeaways

- Modules are files. The module name matches the filename.
- Functions are identified by name *and* arity
- Multiple clauses give you pattern-matched dispatch
- The export list is your public API
- Funs are anonymous functions, great for higher-order programming
- `-spec` annotations help catch bugs with Dialyzer
- Erlang's punctuation (`;` `.` `,`) matters — learn it, love it

You now know enough to write real Erlang programs. Not just shell one-liners, but actual modules with multiple functions, pattern-matched clauses, and clean public APIs. Time to tackle recursion.

---

[← Previous: Pattern Matching](06-pattern-matching.md) | [Next: Recursion →](08-recursion.md)
