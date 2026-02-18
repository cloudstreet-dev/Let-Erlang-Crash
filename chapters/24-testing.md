# Chapter 24: Testing Erlang: Yes, We Test Things

> *"But if you just let things crash, why bother testing?" Great question. Terrible reasoning. "Let it crash" means your system recovers from failure gracefully. It doesn't mean your business logic is correct. A process that crashes and restarts is fine. A process that calculates 2 + 2 = 5 and happily returns it to the user is not fine. That's why we test.*

---

## The Testing Landscape

Erlang has a surprisingly complete testing ecosystem:

| Tool | Type | Ships With OTP? |
|------|------|----------------|
| **EUnit** | Unit testing | Yes |
| **Common Test** | Integration/system testing | Yes |
| **PropEr** | Property-based testing | No (but widely used) |
| **Dialyzer** | Static type analysis | Yes |
| **Meck** | Mocking | No |

## EUnit: Fast Unit Tests

EUnit is built into OTP and is dead simple to use.

### Inline Tests

```erlang
-module(math_utils).
-export([factorial/1, fibonacci/1]).

%% Include EUnit macros
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 -> fibonacci(N-1) + fibonacci(N-2).

%% Tests (only compiled when TEST is defined)
-ifdef(TEST).

factorial_test() ->
    ?assertEqual(1, factorial(0)),
    ?assertEqual(1, factorial(1)),
    ?assertEqual(120, factorial(5)),
    ?assertEqual(3628800, factorial(10)).

fibonacci_test() ->
    ?assertEqual(0, fibonacci(0)),
    ?assertEqual(1, fibonacci(1)),
    ?assertEqual(55, fibonacci(10)).

factorial_negative_test() ->
    ?assertError(function_clause, factorial(-1)).

-endif.
```

Run them:

```erlang
1> c(math_utils, [debug_info, {d, 'TEST'}]).
{ok,math_utils}
2> eunit:test(math_utils).
  All 3 tests passed.
ok
```

Or with rebar3:

```bash
rebar3 eunit
```

### EUnit Assertions

```erlang
?assertEqual(Expected, Actual).          %% Exact match
?assertNotEqual(Unexpected, Actual).     %% Not equal
?assert(BoolExpression).                 %% Must be true
?assertNot(BoolExpression).              %% Must be false
?assertMatch({ok, _}, Expression).       %% Pattern match
?assertError(Pattern, Expression).       %% Must throw error
?assertExit(Pattern, Expression).        %% Must exit
?assertThrow(Pattern, Expression).       %% Must throw
?assertException(Class, Pattern, Expr).  %% General exception
```

### Test Generators

For more complex test organization, use test generators:

```erlang
my_test_() ->
    [
        {"addition works",
         fun() -> ?assertEqual(4, 2 + 2) end},
        {"subtraction works",
         fun() -> ?assertEqual(0, 2 - 2) end}
    ].

%% Setup and teardown
with_setup_test_() ->
    {setup,
     fun() -> kv_store:start_link() end,     %% Setup
     fun(_) -> ok end,                        %% Teardown
     fun(_) ->                                %% Tests
         [
             ?_assertEqual(ok, kv_store:put(a, 1)),
             ?_assertEqual(1, kv_store:get(a))
         ]
     end}.

%% For each: run setup/teardown for EACH test
for_each_test_() ->
    {foreach,
     fun() -> ets:new(test_table, [named_table]) end,
     fun(_) -> ets:delete(test_table) end,
     [
         fun(_) -> ?_assert(ets:insert(test_table, {key, val})) end,
         fun(_) -> ?_assertEqual([], ets:lookup(test_table, missing)) end
     ]}.
```

The `?_assert*` macros (with underscore) create lazy test functions — they're evaluated by the test runner, not immediately.

### Testing GenServers

```erlang
-module(counter_test).
-include_lib("eunit/include/eunit.hrl").

counter_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(Pid) ->
         [
             {"starts at zero",
              fun() -> ?assertEqual(0, counter:get(Pid)) end},
             {"increments",
              fun() ->
                  counter:increment(Pid),
                  counter:increment(Pid),
                  ?assertEqual(2, counter:get(Pid))
              end}
         ]
     end}.

start() ->
    {ok, Pid} = counter:start_link(),
    Pid.

stop(Pid) ->
    gen_server:stop(Pid).
```

## Common Test: The Big Guns

Common Test is OTP's integration testing framework. It's heavier than EUnit but more powerful:

```erlang
%% test/my_app_SUITE.erl
-module(my_app_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_basic_flow/1, test_error_handling/1]).

all() ->
    [test_basic_flow, test_error_handling].

init_per_suite(Config) ->
    application:ensure_all_started(my_app),
    Config.

end_per_suite(_Config) ->
    application:stop(my_app).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test_basic_flow(_Config) ->
    {ok, Id} = my_app:create_thing("test"),
    {ok, Thing} = my_app:get_thing(Id),
    "test" = maps:get(name, Thing),
    ok = my_app:delete_thing(Id).

test_error_handling(_Config) ->
    {error, not_found} = my_app:get_thing(999).
```

Run with rebar3:

```bash
rebar3 ct
```

Common Test features:
- **Suite-level setup/teardown** — `init_per_suite`, `end_per_suite`
- **Test-level setup/teardown** — `init_per_testcase`, `end_per_testcase`
- **Groups** — Organize tests with shared config
- **HTML reports** — Beautiful test reports
- **Distributed testing** — Run tests across nodes
- **Config files** — External test configuration

## Property-Based Testing with PropEr

Property-based testing is where Erlang's testing really shines. Instead of writing specific examples, you describe *properties* that should always hold, and the framework generates random inputs to find counterexamples.

Install PropEr in `rebar.config`:

```erlang
{deps, [{proper, "1.4.0"}]}.
```

### Basic Properties

```erlang
-module(prop_math).
-include_lib("proper/include/proper.hrl").

%% Reversing a list twice gives the original
prop_reverse_reverse() ->
    ?FORALL(L, list(integer()),
        lists:reverse(lists:reverse(L)) =:= L
    ).

%% Sorting is idempotent
prop_sort_idempotent() ->
    ?FORALL(L, list(integer()),
        lists:sort(L) =:= lists:sort(lists:sort(L))
    ).

%% A sorted list has each element <= the next
prop_sort_ordered() ->
    ?FORALL(L, list(integer()),
        is_ordered(lists:sort(L))
    ).

is_ordered([]) -> true;
is_ordered([_]) -> true;
is_ordered([A, B | Rest]) -> A =< B andalso is_ordered([B | Rest]).
```

```erlang
1> proper:quickcheck(prop_math:prop_reverse_reverse()).
....................................................................................................
OK: Passed 100 test(s).
true
```

### Custom Generators

```erlang
%% Generate user maps
user_gen() ->
    ?LET({Name, Age, Email},
         {non_empty(binary()), range(1, 120), binary()},
         #{name => Name, age => Age, email => Email}).

%% Generate valid IP addresses
ip_gen() ->
    ?LET({A, B, C, D},
         {range(0, 255), range(0, 255), range(0, 255), range(0, 255)},
         {A, B, C, D}).
```

### Stateful Testing

PropEr can test stateful systems by generating sequences of operations:

```erlang
%% Test that our counter GenServer behaves like a model
prop_counter() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            {ok, Pid} = counter:start_link(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            counter:stop(Pid),
            ?WHENFAIL(
                io:format("History: ~p~nState: ~p~n", [History, State]),
                Result =:= ok)
        end).
```

This generates random sequences of increment/decrement/get operations and checks that the counter always matches an in-memory model.

## Dialyzer: Static Analysis

Dialyzer (DIscrepancy AnalYzer for ERlang) finds type errors without running your code:

```bash
# Build the PLT (Persistent Lookup Table) — do this once
dialyzer --build_plt --apps erts kernel stdlib

# Analyze your project
rebar3 dialyzer
```

Dialyzer catches things like:

```erlang
%% Dialyzer warns: function never returns 'ok'
bad_function() ->
    case random() of
        true -> error;
        false -> error
    end.

%% Dialyzer warns: pattern can never match
impossible_match(X) when is_integer(X) ->
    case X of
        "hello" -> ok  %% String can't match an integer!
    end.
```

Dialyzer is not a type checker — it's a "discrepancy analyzer." It only reports errors it's *sure* about. If Dialyzer says there's a bug, there is definitely a bug.

## Testing Concurrent Code

Testing concurrent code is tricky. Here are some patterns:

### Synchronous wrappers for testing

```erlang
%% In production: async
handle_request(Req) ->
    gen_server:cast(worker, {process, Req}).

%% For testing: sync version
handle_request_sync(Req) ->
    gen_server:call(worker, {process, Req}).
```

### Using monitors to detect crashes

```erlang
test_crash_recovery() ->
    {ok, Pid} = my_server:start_link(),
    Ref = monitor(process, Pid),
    %% Trigger a crash
    my_server:do_something_bad(),
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 1000 -> error(didnt_crash)
    end,
    %% Verify it restarted (supervisor should handle this)
    timer:sleep(100),
    ?assertNotEqual(undefined, whereis(my_server)).
```

## rebar3 Test Commands

```bash
rebar3 eunit                    # Run EUnit tests
rebar3 ct                       # Run Common Test suites
rebar3 proper                   # Run PropEr tests (with rebar3_proper plugin)
rebar3 dialyzer                 # Run Dialyzer
rebar3 cover                    # Generate coverage report
rebar3 as test shell            # Shell with test config
```

## Key Takeaways

- EUnit for fast unit tests — inline in modules or separate test files
- Common Test for integration tests with setup/teardown
- PropEr for property-based testing — finds edge cases you'd never think of
- Dialyzer for static analysis — catches type errors without running code
- Test GenServers with setup/teardown to start/stop the process
- Property-based testing is Erlang's testing superpower
- Concurrent code needs special patterns (sync wrappers, monitors)
- `rebar3` ties it all together

"Let it crash" is about system-level resilience. Testing is about logic-level correctness. You need both. A system that recovers from crashes but returns wrong answers is not a good system. Test your logic, let the supervisors handle the rest.

---

[← Previous: Ports and NIFs](23-ports-and-nifs.md) | [Next: Real-World Erlang →](25-real-world-erlang.md)
