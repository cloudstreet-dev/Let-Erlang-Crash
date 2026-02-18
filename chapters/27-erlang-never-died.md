# Chapter 27: Erlang Never Died

> *Every few years, someone writes a blog post titled "Is Erlang Dead?" The answer has been "no" every single time. And it will continue to be "no" for a very long time. Here's why.*

---

## The Reports of Erlang's Death

Erlang has been declared dead, dying, or irrelevant roughly once a year since the mid-2000s. The pattern is always the same:

1. Someone discovers Erlang exists
2. They notice it's not on the Stack Overflow Developer Survey's top 10
3. They write "Is Erlang Dead?"
4. The Erlang community quietly continues building systems that handle billions of messages per day

The "is it dead?" question reveals a misunderstanding of what makes a language alive. Erlang was never going to be the next JavaScript. It was never trying to be. It has a specific purpose, and for that purpose, it remains unmatched.

## What "Alive" Actually Means

A language is alive if:
- Active development continues (OTP 27 shipped in 2024 with the new JIT and more)
- Companies are hiring for it (they are)
- Production systems are running on it (billions of them)
- New projects choose it (they do)
- The community is engaged (conferences, meetups, forums, active mailing list)

By every meaningful metric, Erlang is alive and healthy.

## The OTP Release Train Keeps Rolling

Recent OTP releases have brought significant improvements:

**OTP 24 (2021):** BeamAsm JIT compiler — major performance boost for all BEAM code.

**OTP 25 (2022):** Selectable features system, improved maps performance, new documentation system.

**OTP 26 (2023):** Compiler and JIT improvements, better error messages, map comprehensions.

**OTP 27 (2024):** Documentation attributes, triple-quoted strings, improved process aliases.

The BEAM team at Ericsson is actively improving the runtime. The JIT compiler alone represented a fundamental leap in performance. This is not a dead project.

## The BEAM Ecosystem Is Growing

Even as some measure Erlang's popularity in isolation, the BEAM ecosystem as a whole is thriving:

- **Elixir** brought thousands of new developers to the BEAM
- **Phoenix LiveView** pioneered a new approach to real-time web UIs
- **Gleam** is attracting developers who want static types
- **Nerves** is pushing the BEAM into IoT and embedded systems
- **Nx** is bringing numerical computing to the BEAM
- **Livebook** is bringing interactive notebooks to Elixir

The BEAM has never had more languages, more developers, or more use cases than it does right now. Measuring only Erlang-the-language misses the forest for the trees.

## Why Erlang Keeps Winning

### The Problems It Solves Haven't Gone Away

The world needs more concurrent, distributed, fault-tolerant systems — not fewer. Every year brings:
- More connected devices (IoT)
- More real-time communication (chat, video, collaboration)
- More distributed systems (microservices, edge computing)
- More demand for reliability (fintech, healthcare, infrastructure)

These are Erlang's core strengths. The industry's direction is toward the problems Erlang has been solving for decades.

### Nothing Else Has Replaced It

Name another runtime that gives you all of this:
- Millions of lightweight processes
- Per-process garbage collection
- Preemptive scheduling
- Process isolation
- Built-in distribution
- Hot code reloading
- Battle-tested supervision trees

Go has goroutines but no supervision trees and no hot code reloading. Rust has memory safety but no lightweight processes. Java has virtual threads now but no per-process GC and no built-in distribution. Node.js has... npm.

Some languages have pieces of what the BEAM provides. None have the whole package. And the whole package is what matters when you're building systems that need to run for years.

### The Lindy Effect

The Lindy effect says that the longer a technology has been around, the longer it's likely to continue. Erlang has been in continuous production use since 1986. The codebases running on it are massive, critical, and expensive to replace. The knowledge base is deep. The tools are mature.

Technologies die when their use cases go away or when something strictly better replaces them. Erlang's use cases are growing, and nothing has replaced it.

## What Joe Armstrong Left Behind

Joe Armstrong passed away in April 2019. His contributions to computer science go far beyond Erlang:

- The idea that programming should be about building reliable systems, not clever algorithms
- The "let it crash" philosophy as a practical engineering principle
- A deep understanding that concurrency is not a feature to add but a foundation to build on
- The conviction that simplicity and reliability are more important than performance benchmarks

His Ph.D. thesis, "Making reliable distributed systems in the presence of software errors," remains one of the most practical and insightful documents in the field. It's freely available online and absolutely worth reading.

Joe's approach to programming was human-centered: build systems that work in the real world, where everything fails, networks are unreliable, and bugs are inevitable. Design for the messy reality, not the idealized theory.

## Where Erlang Goes From Here

The BEAM platform is expanding in several directions:

**Performance:** The JIT compiler continues to improve. Erlang is faster than ever, and future OTP releases will push performance further.

**Usability:** Better error messages, better tooling, better documentation. The ecosystem is becoming more approachable.

**Ecosystem diversity:** Gleam brings static types. Elixir brings modern syntax. LFE brings Lisp. Each language attracts different developers to the BEAM.

**New domains:** IoT (Nerves), data processing (Broadway), machine learning (Nx). The BEAM is finding new homes beyond its traditional telecom roots.

**Community:** The Erlang Ecosystem Foundation (EEF) coordinates community efforts. Conferences like Code BEAM and ElixirConf keep the community connected.

## The Closing Argument

Erlang isn't the most popular language. It's not the trendiest. It doesn't have the flashiest syntax or the most packages on its package manager.

But when you need a system that:
- Handles millions of concurrent connections
- Stays up for years without restart
- Recovers from failures automatically
- Scales across machines transparently
- Gets upgraded without downtime

...there is no better tool.

Erlang wasn't designed to win popularity contests. It was designed to build systems that don't go down. And at that job, nearly four decades later, it's still the best in the world.

## What to Do Next

You've made it through the book. Here's what to do now:

1. **Build something.** A chat server. A key-value store. A job queue. Pick a project and use what you've learned.

2. **Read the OTP source code.** It's all Erlang, and it's remarkably well-written. Start with `gen_server.erl` — it's a masterclass in clean code.

3. **Join the community.** The Erlang Forums, the Erlang Slack, the erlang-questions mailing list, and the EEF are all welcoming places.

4. **Try Elixir.** If you haven't already, explore the BEAM from the Elixir side. Everything you learned here transfers directly.

5. **Read Joe Armstrong's thesis.** "Making reliable distributed systems in the presence of software errors." It's the document that captures everything Erlang is about.

6. **Ship something to production.** The real learning starts when your system has actual users and actual failures.

---

## Thank You

Thanks for reading *Let Erlang Crash*. If this book made you curious about Erlang, or helped you understand something that was confusing, or made you laugh even once, it did its job.

Now go spawn some processes and let them crash.

---

*This book is licensed [CC0](../LICENSE) — public domain. Share it, remix it, teach from it. The BEAM community is better when more people understand what makes it special.*

---

[← Previous: Elixir](26-elixir.md) | [Back to Table of Contents →](../README.md)
