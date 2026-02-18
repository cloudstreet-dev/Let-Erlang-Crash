# Chapter 25: Real-World Erlang: WhatsApp, RabbitMQ, and Friends

> *Theory is nice. Academic papers are lovely. But the real test of a programming language is: does anyone actually build serious stuff with it? Erlang doesn't just pass this test — it crushes it. Some of the most demanding, highest-scale, most reliability-critical systems on the planet run on the BEAM. Let's talk about them.*

---

## WhatsApp: The Poster Child

When Facebook acquired WhatsApp in 2014 for $19 billion, it was the largest acquisition of a venture-backed company in history. At that point, WhatsApp handled 450 million monthly users with a team of about 50 engineers.

The backend was Erlang.

**The numbers:**
- ~2 million TCP/IP connections per server
- 50+ billion messages per day at peak
- Engineering team of ~50 people (for the backend)
- Custom fork of ejabberd (an Erlang XMPP server)

**Why Erlang worked:** WhatsApp needed massive concurrency (millions of persistent connections), soft real-time message delivery, and extreme reliability. Each connection was an Erlang process. Message routing was message passing. Crash recovery was supervisors. The entire architecture mapped naturally onto BEAM primitives.

The efficiency was remarkable — WhatsApp achieved a user-to-engineer ratio that was orders of magnitude better than comparable services, largely because Erlang handled concurrency and fault tolerance at the platform level.

## RabbitMQ: The Message Broker

RabbitMQ is one of the most widely deployed message brokers in the world. It implements the AMQP protocol and is used by thousands of companies for everything from microservice communication to IoT data pipelines.

Written in Erlang.

**Why Erlang was the right choice:**
- Message routing is literally what Erlang was designed for
- Each connection/channel is a process
- Queues are processes with mailboxes
- Clustering uses distributed Erlang
- Plugin system leverages OTP application architecture
- Hot code reloading enables zero-downtime upgrades

RabbitMQ's architecture is essentially Erlang's concurrency model expressed as a message broker. Queues are processes. Exchanges route messages to queues. Connections are processes that talk to queue processes. It's BEAM processes all the way down.

## Ericsson: Where It All Began

Ericsson developed Erlang and still uses it extensively. Their AXD 301 ATM switch was one of the earliest major Erlang systems, achieving "nine nines" reliability — 99.9999999% uptime, or about 31 milliseconds of downtime per year.

Ericsson handles a significant portion of the world's mobile telecom traffic on Erlang-based systems. These systems have been running — and being upgraded via hot code reloading — for decades.

## Discord: Elixir on the BEAM

Discord uses Elixir (which runs on the BEAM) for their real-time messaging infrastructure. At scale, they handle millions of concurrent users across thousands of guild servers.

Their engineering blog has documented several interesting BEAM-specific solutions:
- Using ETS for fast, concurrent data access
- Manifold for efficient pub/sub within a single node
- Custom process management for guild processes

Discord demonstrates that the BEAM's strengths extend beyond Erlang itself — the virtual machine is the platform, and both Erlang and Elixir benefit from it.

## Klarna: Fintech at Scale

Klarna, one of Europe's largest fintech companies, has used Erlang since its founding. Their payment processing system handles enormous transaction volumes with the reliability guarantees that financial services demand.

In fintech, a crash can mean lost money. Klarna's use of Erlang's fault-tolerance features — supervisors, process isolation, hot code upgrades — means that individual failures don't propagate into financial errors.

## CouchDB and CouchBase

CouchDB, the distributed document database, was originally written in Erlang. Its replication protocol, conflict resolution, and clustering all leveraged Erlang's distributed computing primitives.

The design philosophy matched perfectly: CouchDB was built around eventual consistency, where nodes operate independently and sync when possible — exactly the kind of distributed system Erlang excels at.

## Bet365: Real-Time Betting

Bet365, one of the world's largest online gambling companies, uses Erlang extensively for their real-time betting platform. They need:
- Millions of concurrent connections
- Sub-second odds updates
- Zero downtime (bets never stop)
- Consistent state across distributed systems

Sound familiar? These are exactly the requirements Erlang was designed for.

## Cisco: Network Infrastructure

Cisco uses Erlang in their network management and configuration tools, including parts of their NSO (Network Services Orchestrator) platform. Network infrastructure has the same uptime requirements as telecom switches — the network can't go down.

## VerneMQ and EMQ X: MQTT Brokers

The IoT world runs on MQTT, and two of the most popular MQTT brokers are built on the BEAM:

- **VerneMQ** — Written in Erlang
- **EMQX** — Written in Erlang

IoT means millions of devices maintaining persistent connections, sending small messages frequently. Sound like a telecom switch? Same problem, different era.

## What These Systems Have in Common

Every system above shares these characteristics:

1. **Massive concurrency** — Millions of connections or concurrent operations
2. **Persistent connections** — Long-lived TCP connections, not just request/response
3. **Soft real-time** — Responses needed in milliseconds, not seconds
4. **High availability** — Downtime is unacceptable or extremely costly
5. **Message-oriented** — The core operation is routing messages between entities

These are the problems Erlang was born to solve. Not because it's the fastest language, or the most expressive, or the most popular. But because its concurrency model, fault tolerance, and distribution primitives map directly onto these requirements.

## The Pattern

If you look at successful Erlang/BEAM deployments, there's a clear pattern:

```
Problem domain              → BEAM solution
─────────────────────────────────────────────
Per-connection state        → One process per connection
Message routing             → Message passing between processes
Crash isolation             → Process isolation + supervisors
Horizontal scaling          → Distributed Erlang
Zero-downtime upgrades      → Hot code reloading
Persistent connections      → Lightweight processes (millions)
Real-time responsiveness    → Preemptive scheduling, per-process GC
```

Each row is a hard problem in most languages. In Erlang, each one is a built-in feature.

## Why Not Erlang?

Being fair, Erlang is not the best choice for everything:

- **CPU-intensive computation** — Number crunching, ML training, video encoding. Use C, Rust, or specialized tools.
- **GUI applications** — Erlang has no native GUI story (though wxWidgets bindings exist).
- **Single-threaded scripting** — Python, Ruby, or Bash are simpler for small scripts.
- **Ecosystem depth** — JavaScript/Python have more libraries for web frontends, data science, etc.

Erlang is a specialist tool for distributed, concurrent, fault-tolerant systems. In that domain, nothing else comes close.

## Key Takeaways

- WhatsApp proved Erlang can handle billions of messages with a tiny team
- RabbitMQ showed that message brokers map naturally to Erlang's model
- Ericsson achieved "nine nines" reliability with Erlang telephone switches
- Discord, Klarna, Bet365, Cisco all use BEAM for demanding production systems
- The pattern: massive concurrency + persistent connections + high availability = BEAM
- Erlang isn't for everything, but for its niche, nothing matches it

The next time someone asks "who uses Erlang?", you can tell them: the systems they use every day. They just don't know it.

---

[← Previous: Testing](24-testing.md) | [Next: Elixir →](26-elixir.md)
