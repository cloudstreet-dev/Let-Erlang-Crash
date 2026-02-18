# Chapter 22: Distributed Erlang: Nodes Talking to Nodes

> *Most distributed systems require message queues, service meshes, API gateways, and a PhD in YAML configuration. In Erlang, you connect two nodes and start sending messages. That's it. The same `Pid ! Message` syntax works whether the process is on your machine or on a server across the ocean. Distribution is built into the language so deeply that processes genuinely don't know or care where other processes live.*

---

## What Is a Node?

A node is a running BEAM instance with a name. When you start Erlang with a name, it becomes a node that other nodes can connect to:

```bash
# Start a named node
erl -sname alice
```

```
Erlang/OTP 27 [erts-14.0]

Eshell V14.0
(alice@hostname)1>
```

That `(alice@hostname)` prompt tells you you're on a named node. There are two naming modes:

- `-sname` — Short names: `alice@hostname` (same network)
- `-name` — Long names: `alice@192.168.1.100` (any network, requires DNS or IPs)

## Connecting Nodes

Open two terminals:

```bash
# Terminal 1
erl -sname alice -setcookie secret

# Terminal 2
erl -sname bob -setcookie secret
```

The cookie is a shared secret for authentication. All nodes in a cluster must use the same cookie.

From Alice, connect to Bob:

```erlang
%% On alice:
(alice@hostname)1> net_adm:ping('bob@hostname').
pong
```

`pong` means success. `pang` means failure. They're now connected. Check:

```erlang
(alice@hostname)2> nodes().
['bob@hostname']
```

## Sending Messages Across Nodes

```erlang
%% On bob: register a process
(bob@hostname)1> register(echo, spawn(fun Loop() ->
(bob@hostname)1>     receive
(bob@hostname)1>         {From, Msg} ->
(bob@hostname)1>             From ! {echo, Msg},
(bob@hostname)1>             Loop()
(bob@hostname)1>     end
(bob@hostname)1> end)).
true

%% On alice: send a message to bob's process
(alice@hostname)1> {echo, 'bob@hostname'} ! {self(), "hello from alice!"}.
{<0.85.0>,"hello from alice!"}
(alice@hostname)2> flush().
Shell got {echo,"hello from alice!"}
ok
```

The syntax `{RegisteredName, Node} ! Message` sends a message to a named process on another node. The process on Bob received the message, sent a reply, and Alice got it. Just like local message passing, but across machines.

## Spawning Processes on Remote Nodes

```erlang
%% Spawn a process on bob from alice
(alice@hostname)1> Pid = spawn('bob@hostname', fun() ->
(alice@hostname)1>     io:format("I'm running on ~p!~n", [node()])
(alice@hostname)1> end).
<12345.89.0>
%% "I'm running on bob@hostname!" prints on bob's console
```

The PID format changes — that first number (12345) is the node identifier, not 0.

## Location Transparency

This is the magic: the same code works regardless of whether processes are local or remote:

```erlang
%% This function works with any Pid — local or remote
send_work(Pid, Work) ->
    Pid ! {work, self(), Work},
    receive
        {result, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.
```

The process at `Pid` could be on the same node, on another node in the same data center, or on a node across the world. The code doesn't change.

## Global Process Registry

For registered names across a cluster:

```erlang
%% Register globally (visible to all connected nodes)
global:register_name(my_global_server, self()).

%% Find it from any node
global:whereis_name(my_global_server).
%% <12345.89.0>

%% Send to it
global:send(my_global_server, hello).
```

Or use `{global, Name}` with GenServer:

```erlang
gen_server:start_link({global, my_server}, ?MODULE, [], []).

%% Call from any node:
gen_server:call({global, my_server}, request).
```

## The Mesh Network

Erlang nodes form a fully connected mesh by default. When Alice connects to Bob, and Bob is already connected to Carol, Alice automatically connects to Carol too:

```
Before alice connects to bob:
Alice    Bob ←→ Carol

After:
Alice ←→ Bob ←→ Carol
  ↑                ↑
  └────────────────┘
  (auto-connected)
```

This is called "transitive connection" and it happens automatically. You can disable it with `net_kernel:allow/1` if you need to control the topology.

## Monitoring Remote Nodes

```erlang
%% Get notified when a node goes down
net_kernel:monitor_nodes(true).

%% Now if bob disconnects:
receive
    {nodedown, 'bob@hostname'} ->
        io:format("Bob is gone!~n")
end.

%% Also works for node coming up:
receive
    {nodeup, 'bob@hostname'} ->
        io:format("Bob is back!~n")
end.
```

You can also monitor remote processes with the regular `monitor/2`:

```erlang
Ref = monitor(process, {echo, 'bob@hostname'}).
%% If bob's echo process dies OR bob disconnects:
receive
    {'DOWN', Ref, process, _, Reason} ->
        io:format("Echo is down: ~p~n", [Reason])
end.
```

## A Distributed Key-Value Store

Let's build a simple distributed KV store that replicates writes to all nodes:

```erlang
-module(dist_kv).
-behaviour(gen_server).
-export([start_link/0, put/2, get/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
    %% Write to all nodes
    [gen_server:cast({?MODULE, Node}, {put, Key, Value})
     || Node <- [node() | nodes()]],
    ok.

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

init([]) ->
    {ok, #{}}.

handle_call({get, Key}, _From, State) ->
    {reply, maps:get(Key, State, undefined), State}.

handle_cast({put, Key, Value}, State) ->
    {noreply, State#{Key => Value}}.
```

```erlang
%% On alice:
dist_kv:start_link().
dist_kv:put(greeting, "hello world").

%% On bob (assuming dist_kv is started):
dist_kv:get(greeting).
%% "hello world"
```

Writes are broadcast to all nodes. Reads are local. This is an eventually-consistent replicated store in about 20 lines.

## The CAP Theorem and Erlang

Erlang's distribution is designed for partition tolerance and availability. When the network splits:

- Nodes continue running independently
- `nodedown` messages notify processes of disconnections
- It's up to your application to decide what to do

Mnesia, for example, can be configured for different split-brain strategies. Some applications use quorum-based approaches, others designate a master node, and others simply halt affected subsystems until the partition heals.

## Security

The cookie-based authentication is basic. For production:

```bash
# Use TLS for inter-node communication
erl -proto_dist inet_tls \
    -ssl_dist_optfile ssl_dist.conf \
    -sname alice
```

Configure TLS certificates in `ssl_dist.conf`:

```erlang
[{server, [{certfile, "server.pem"},
           {keyfile, "server-key.pem"},
           {cacertfile, "ca.pem"},
           {verify, verify_peer}]},
 {client, [{certfile, "client.pem"},
           {keyfile, "client-key.pem"},
           {cacertfile, "ca.pem"},
           {verify, verify_peer}]}].
```

## Hidden Nodes

Sometimes you want a node that monitors the cluster without being part of it:

```bash
erl -sname monitor -hidden -setcookie secret
```

Hidden nodes don't appear in `nodes()` and don't form automatic connections. Useful for debugging, monitoring, and maintenance tools.

## Key Takeaways

- Nodes are named BEAM instances that can connect to each other
- `Pid ! Message` works identically for local and remote processes
- Nodes form a fully connected mesh by default
- Same cookie = trust; use TLS for real security
- `global` registry for cluster-wide process naming
- `monitor_nodes/1` for tracking node availability
- Hidden nodes for monitoring without disrupting the cluster
- The CAP theorem applies — plan your partition strategy

Distribution in Erlang isn't an afterthought bolted on with libraries. It's baked into the runtime at the lowest level. The same PID type, the same send operator, the same monitor mechanism — everything just works across nodes. This is why Erlang is the go-to for systems that need to span multiple machines without the complexity of typical distributed systems tooling.

---

[← Previous: Hot Code Reloading](21-hot-code-reloading.md) | [Next: Ports and NIFs →](23-ports-and-nifs.md)
