# Chapter 3: The Shell: Erlang's REPL of Wonders

> *Most REPLs are polite little things where you type expressions and get answers. The Erlang shell is a distributed, job-managing, process-spawning interactive runtime that also happens to let you type expressions and get answers. It's like if your Python REPL could hot-reload production code across a cluster of machines.*

---

## Welcome to Eshell

Start it up:

```bash
erl
```

```
Erlang/OTP 27 [erts-14.0] [source] [64-bit] [smp:8:8]

Eshell V14.0 (press Ctrl+G to quit)
1>
```

That `1>` is your prompt. The number increments with each expression. Let's play.

## The Basics

```erlang
1> 1 + 1.
2
2> "hello" ++ " " ++ "world".
"hello world"
3> lists:seq(1, 10).
[1,2,3,4,5,6,7,8,9,10]
4> length(lists:seq(1, 1000000)).
1000000
```

Every expression ends with a period (`.`). This is not optional. If you forget it, the shell just sits there, waiting, judging you silently. When this happens — and it will — just type a period and hit Enter.

## Variables (They Start with Capital Letters)

```erlang
1> X = 42.
42
2> X.
42
3> X = 43.
** exception error: no match of right hand side value 43
```

Whoa. What just happened? `X` is already bound to `42`. In Erlang, `=` is not assignment — it's *pattern matching*. Once a variable is bound, it stays bound. We'll dig deep into this in Chapter 4. For now, just know that variables start with uppercase letters and they don't change.

To start fresh, use `f()` to forget all bindings:

```erlang
4> f().
ok
5> X = 43.
43
```

Or forget a specific variable:

```erlang
6> f(X).
ok
```

## Atoms: Erlang's Favourite Thing

```erlang
1> hello.
hello
2> is_atom(hello).
true
3> is_atom('Hello World').
true
```

Atoms are constants where the name *is* the value. Think of them as self-describing labels. They're everywhere in Erlang. We'll cover them properly in Chapter 5.

## Calling Module Functions

Erlang's standard library is organized into modules. You call functions with `Module:Function(Args)` syntax:

```erlang
1> lists:reverse([1, 2, 3]).
[3,2,1]
2> string:uppercase("hello").
"HELLO"
3> math:pi().
3.141592653589793
4> erlang:system_info(process_count).
42
```

## Shell Built-in Commands

The shell has a bunch of handy commands. Here are the ones you'll actually use:

```erlang
%% Help
1> help().

%% Forget all variable bindings
2> f().

%% See command history
3> h().

%% Reuse the result of expression N
4> v(1).

%% Compile a module
5> c(my_module).

%% Load module info
6> m(lists).

%% Show all loaded modules
7> m().

%% Get info about a process (self() is the shell process)
8> process_info(self()).
```

## The Miracle of Tab Completion

The Erlang shell supports tab completion, and it's incredibly useful:

```
1> li<TAB>
lists    lib
1> lists:<TAB>
lists:all/2         lists:any/2         lists:append/1     ...
```

It completes module names, function names, and even shows arities (the number of arguments). This is your documentation browser when you can't remember function names.

## Shell Jobs (Yes, Really)

Here's where it gets weird. The Erlang shell supports multiple concurrent jobs. Press `Ctrl+G`:

```
User switch command
 --> h
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
 --> s
 --> j
   1  {shell,start,[init]}
   2* {shell,start,[]}
 --> c 2
```

You just started a second shell session inside the same BEAM instance. Each shell is its own Erlang process. You can even connect to a remote node's shell from here — but we'll save that for the distributed Erlang chapter.

## Connecting to a Running Node

This is one of Erlang's party tricks. You can connect to a running Erlang system and get a live shell:

```bash
# Start a named node
erl -name myapp@127.0.0.1 -setcookie mysecret

# From another terminal, connect to it
erl -name debug@127.0.0.1 -setcookie mysecret -remsh myapp@127.0.0.1
```

You're now inside a live running system. You can inspect processes, check state, call functions, even reload code. In production. While it's running. This is the superpower that ops people don't believe until they see it.

## Useful Shell Tricks

### Multi-line expressions

Just don't put a period until you're done:

```erlang
1> X = case 1 + 1 of
1>     2 -> "math works";
1>     _ -> "uh oh"
1> end.
"math works"
```

### Quick anonymous functions

```erlang
1> Double = fun(X) -> X * 2 end.
#Fun<erl_eval.44.79398840>
2> Double(21).
42
```

### Timing things

```erlang
1> timer:tc(fun() -> lists:seq(1, 1000000) end).
{38521,
 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
  23,24,25,26,...]}
```

That first number is microseconds. Your million-element list took about 38 milliseconds. Not bad.

### Getting out

- `q().` — graceful shutdown (stops the entire BEAM)
- `Ctrl+G` then `q` — same thing
- `Ctrl+C` then `a` — abort (less graceful)
- `Ctrl+C` twice — emergency kill

`q()` is preferred because it lets the system shut down properly. Double `Ctrl+C` is the "get me out of here *now*" option.

## The Shell Is a Process

Here's a mind-bending fact: the Erlang shell is itself an Erlang process. It has a PID, it receives messages, and it can crash:

```erlang
1> self().
<0.85.0>
2> 1/0.
** exception error: an error occurred when evaluating
                    an arithmetic expression
     in operator  '/'/2
        called as 1 / 0
3> self().
<0.89.0>
```

Notice that `self()` returned a different PID after the crash. The shell process died and was restarted by its supervisor. *You just witnessed the "let it crash" philosophy in action*, and you haven't even left the REPL yet.

## What We Learned

- The Erlang shell is a powerful interactive runtime, not just a calculator
- Variables are single-assignment (we'll explore why later)
- `Module:Function(Args)` is how you call things
- Tab completion is your friend
- The shell supports multiple jobs and remote connections
- The shell itself is a supervised Erlang process that can crash and restart

The shell is going to be your constant companion as you learn Erlang. Get comfortable with it. Talk to it. Tell it your secrets. It won't judge you (it will just pattern-match against them).

---

[← Previous: Installing Erlang](02-installing-erlang.md) | [Next: Variables Don't Vary →](04-variables.md)
