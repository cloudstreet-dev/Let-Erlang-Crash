# Chapter 26: Elixir: The Cool Younger Sibling

> *In 2011, Jose Valim — a Ruby core team member — discovered the BEAM and thought: "This virtual machine is incredible. The language on top of it could use some work." So he created Elixir: all the power of the BEAM with a syntax that doesn't scare off Ruby and Python developers. The result is one of the most successful language-on-VM stories since Kotlin on the JVM.*

---

## What Is Elixir?

Elixir is a programming language that:
- Runs on the BEAM (same VM as Erlang)
- Has Ruby-influenced syntax
- Adds modern language features (pipe operator, protocols, macros)
- Can call Erlang modules directly (zero cost)
- Has its own ecosystem (Mix, Hex, Phoenix)
- Is fully compatible with OTP

It's not a replacement for Erlang. It's an alternative syntax with extra features, sharing the same runtime.

## Erlang vs. Elixir: Side by Side

### A GenServer

**Erlang:**
```erlang
-module(counter).
-behaviour(gen_server).
-export([start_link/0, increment/0, get/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

increment() -> gen_server:cast(?MODULE, :increment).
get() -> gen_server:call(?MODULE, :get).

init(Initial) -> {ok, Initial}.

handle_cast(:increment, Count) -> {noreply, Count + 1}.

handle_call(:get, _From, Count) -> {reply, Count, Count}.
```

**Elixir:**
```elixir
defmodule Counter do
  use GenServer

  def start_link(initial \\ 0) do
    GenServer.start_link(__MODULE__, initial, name: __MODULE__)
  end

  def increment, do: GenServer.cast(__MODULE__, :increment)
  def get, do: GenServer.call(__MODULE__, :get)

  @impl true
  def init(initial), do: {:ok, initial}

  @impl true
  def handle_cast(:increment, count), do: {:noreply, count + 1}

  @impl true
  def handle_call(:get, _from, count), do: {:reply, count, count}
end
```

The structure is identical. The callbacks are the same. The return values are the same. It's the same OTP GenServer — just with different syntax.

### Pattern Matching

**Erlang:**
```erlang
area({circle, R}) -> math:pi() * R * R;
area({rect, W, H}) -> W * H.
```

**Elixir:**
```elixir
def area({:circle, r}), do: :math.pi() * r * r
def area({:rect, w, h}), do: w * h
```

### Pipe Operator

This is Elixir's most beloved feature. Instead of nested function calls:

```erlang
%% Erlang: read inside-out
string:uppercase(string:trim(binary_to_list(Data))).
```

```elixir
# Elixir: read left-to-right
Data
|> :binary.bin_to_list()
|> String.trim()
|> String.upcase()
```

The pipe operator `|>` passes the result of each expression as the first argument to the next function. It's purely syntactic sugar but dramatically improves readability for data transformation pipelines.

## What Elixir Adds

### Metaprogramming (Macros)

Elixir has a powerful macro system built on Erlang's AST:

```elixir
defmacro unless(condition, do: block) do
  quote do
    if !unquote(condition), do: unquote(block)
  end
end
```

Erlang has parse transforms and macros (`-define`), but Elixir's macro system is more accessible and more commonly used.

### Protocols (Ad-hoc Polymorphism)

```elixir
defprotocol Stringify do
  def to_string(data)
end

defimpl Stringify, for: Integer do
  def to_string(n), do: Integer.to_string(n)
end

defimpl Stringify, for: List do
  def to_string(list), do: Enum.join(list, ", ")
end
```

Similar to Erlang behaviours but dispatched on data type at runtime.

### Sigils and String Interpolation

```elixir
name = "World"
"Hello, #{name}!"       # String interpolation
~r/hello\s+world/       # Regex sigil
~w(foo bar baz)          # Word list sigil
```

Erlang uses `io_lib:format` for string formatting. Elixir's interpolation is more ergonomic.

### Mix: The Build Tool

Mix is Elixir's build tool, inspired by Bundler/Leiningen/Cargo:

```bash
mix new my_app       # Create a project
mix deps.get         # Fetch dependencies
mix compile          # Compile
mix test             # Run tests
mix format           # Auto-format code
```

Erlang has `rebar3`, which serves a similar role.

## The Ecosystem

### Phoenix Framework

Phoenix is to Elixir what Rails is to Ruby — a full-featured web framework. But it runs on the BEAM, which means:
- LiveView for real-time UIs without JavaScript
- Channels for WebSocket communication (think chat, live updates)
- PubSub built on Erlang's distribution
- Presence tracking for knowing who's online

### Hex Package Manager

Hex is the package manager for both Elixir and Erlang. Yes, Erlang packages and Elixir packages live on the same registry and can depend on each other.

```bash
# Elixir packages can depend on Erlang packages
{:cowboy, "~> 2.9"}    # Erlang HTTP server, used from Elixir

# Erlang rebar3 projects can depend on Hex packages too
{deps, [{jsx, "3.1.0"}]}.
```

### Nerves

Nerves is an Elixir framework for embedded systems — running BEAM on IoT devices. Your Raspberry Pi can run Elixir with full OTP supervision.

### Nx and Livebook

Nx is numerical computing for Elixir, with GPU support. Livebook is an interactive notebook (like Jupyter) for Elixir. The BEAM ecosystem is expanding into data science.

## Calling Between Languages

Elixir and Erlang interoperate perfectly:

```elixir
# Calling Erlang from Elixir
:crypto.hash(:md5, "hello")
:ets.new(:my_table, [:set, :public])
:gen_server.call(pid, :request)
```

```erlang
%% Calling Elixir from Erlang
'Elixir.MyModule':my_function(Args).
'Elixir.Phoenix.PubSub':broadcast(PubSub, Topic, Message).
```

Elixir modules are atoms with the `Elixir.` prefix. The interop is zero-cost — they share the same bytecode, the same processes, the same messages.

## Should You Learn Elixir Instead?

**Learn Erlang if:**
- You want to understand the BEAM at its core
- You're working on an existing Erlang codebase
- You value minimalism and simplicity
- You're working in telecom, embedded, or systems programming
- You want to read OTP source code (it's all Erlang)

**Learn Elixir if:**
- You're building web applications
- You come from Ruby, Python, or JavaScript
- You want the largest BEAM community and ecosystem
- You're starting a new project with no existing codebase
- You prefer more syntactic sugar and metaprogramming

**Ideally, learn both.** Understanding Erlang makes you a better Elixir developer, and vice versa. They're the same platform with different accents.

## What About Gleam?

Gleam is the newest BEAM language — statically typed with Rust-inspired syntax:

```gleam
pub fn fibonacci(n: Int) -> Int {
  case n {
    0 -> 0
    1 -> 1
    n -> fibonacci(n - 1) + fibonacci(n - 2)
  }
}
```

Gleam compiles to both BEAM bytecode and JavaScript. It's young but growing fast, especially among developers who want static types on the BEAM.

## Key Takeaways

- Elixir runs on the same BEAM VM and uses the same OTP principles
- The syntax is different; the semantics are identical
- Elixir adds macros, protocols, the pipe operator, and a polished toolchain
- Phoenix is the dominant web framework on the BEAM
- Elixir and Erlang interoperate seamlessly at zero cost
- Gleam adds static typing to the BEAM ecosystem
- The BEAM is the platform; languages are syntactic preferences

The BEAM ecosystem is richer than ever. Whether you write Erlang, Elixir, or Gleam, you get the same legendary runtime — processes, supervisors, distribution, hot code reloading, and decades of battle-tested engineering. Choose the syntax that makes you happy, and let the BEAM handle the hard parts.

---

[← Previous: Real-World Erlang](25-real-world-erlang.md) | [Next: Erlang Never Died →](27-erlang-never-died.md)
