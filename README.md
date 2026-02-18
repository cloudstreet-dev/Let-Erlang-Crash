# Let Erlang Crash

**A fun, irreverent guide to the world's most indestructible programming language.**

**[Read the book online](https://cloudstreet-dev.github.io/Let-Erlang-Crash/)**

---

> *"The problem with object-oriented languages is they've got all this implicit environment that they carry around with them. You wanted a banana but what you got was a gorilla holding the banana and the entire jungle."*
> — Joe Armstrong, creator of Erlang

---

## What Is This?

This is a free, open-source book about Erlang — the programming language that was built to never go down, and somehow also ended up being *fun*.

Erlang was born in 1986 inside Ericsson, designed to run telephone switches that couldn't afford to crash. It's the language behind WhatsApp (handling 2 million connections per server), RabbitMQ, CouchDB, and half the world's telecom infrastructure. It runs on the BEAM virtual machine, which is basically what would happen if someone built a VM specifically to make reliability nerds weep with joy.

This book will teach you Erlang the way it deserves to be taught: with enthusiasm, bad jokes, and a deep appreciation for the beautiful weirdness of a language where crashing is a *feature*.

## Who Is This For?

- Programmers who are curious about Erlang but have been scared off by the syntax
- People who've heard "let it crash" and want to know what the hell that actually means
- Developers drowning in microservices who want to see how the grown-ups do concurrency
- Anyone who enjoys a programming book that doesn't read like a tax form

## Table of Contents

| # | Chapter | Description |
|---|---------|-------------|
| 01 | [Why Erlang? Why Now? Why Ever?](chapters/01-why-erlang.md) | The pitch. Why a language from 1986 is more relevant than your last framework. |
| 02 | [Installing Erlang (The Easy Part)](chapters/02-installing-erlang.md) | Getting the BEAM on your machine without losing your mind. |
| 03 | [The Shell: Erlang's REPL of Wonders](chapters/03-the-shell.md) | Your new favorite place to break things. |
| 04 | [Variables Don't Vary (And That's Fine)](chapters/04-variables.md) | Single assignment, pattern matching, and why mutability was a mistake. |
| 05 | [Atoms, Tuples, and Lists — Oh My](chapters/05-data-types.md) | Erlang's data types are weird and wonderful. |
| 06 | [Pattern Matching: The Superpower](chapters/06-pattern-matching.md) | The single best feature in Erlang (fight me). |
| 07 | [Functions and Modules: Where Code Lives](chapters/07-functions-and-modules.md) | Writing actual programs instead of shell one-liners. |
| 08 | [Recursion: Who Needs Loops?](chapters/08-recursion.md) | Erlang doesn't have for loops. You'll be fine. |
| 09 | [The BEAM: Erlang's Secret Weapon](chapters/09-the-beam.md) | The virtual machine that makes everything possible. |
| 10 | [Processes: Lighter Than Your Threads](chapters/10-processes.md) | Spawning a million things and not even sweating. |
| 11 | [Message Passing: No Shared Memory, No Problems](chapters/11-message-passing.md) | How processes talk without stepping on each other. |
| 12 | [Let It Crash: The Philosophy](chapters/12-let-it-crash.md) | The most misunderstood idea in programming, explained. |
| 13 | [Links and Monitors: Watching Things Die](chapters/13-links-and-monitors.md) | Because if something crashes, *someone* should know about it. |
| 14 | [Error Handling: It's Not What You Think](chapters/14-error-handling.md) | try/catch exists, but you probably shouldn't use it. |
| 15 | [OTP: The Batteries That Are Actually Included](chapters/15-otp.md) | The framework that makes Erlang Erlang. |
| 16 | [GenServer: Your New Best Friend](chapters/16-genserver.md) | The most important behaviour you'll ever learn. |
| 17 | [Supervisors: Let Someone Else Worry](chapters/17-supervisors.md) | Building systems that heal themselves. |
| 18 | [Supervision Trees: It's Supervisors All the Way Down](chapters/18-supervision-trees.md) | Architecting for failure at every level. |
| 19 | [ETS: Stupid Fast In-Memory Storage](chapters/19-ets.md) | When you need a table and you need it *now*. |
| 20 | [Mnesia: The Database That Lives in Your VM](chapters/20-mnesia.md) | A distributed database that's also kind of bonkers. |
| 21 | [Hot Code Reloading: Changing the Engine Mid-Flight](chapters/21-hot-code-reloading.md) | Upgrading production code without stopping. Yes, really. |
| 22 | [Distributed Erlang: Nodes Talking to Nodes](chapters/22-distributed-erlang.md) | Connecting BEAM instances across machines like it's nothing. |
| 23 | [Ports and NIFs: Talking to the Outside World](chapters/23-ports-and-nifs.md) | When you need C, Rust, or other languages to help out. |
| 24 | [Testing Erlang: Yes, We Test Things](chapters/24-testing.md) | EUnit, Common Test, and property-based testing with PropEr. |
| 25 | [Real-World Erlang: WhatsApp, RabbitMQ, and Friends](chapters/25-real-world-erlang.md) | The systems that prove this stuff actually works. |
| 26 | [Elixir: The Cool Younger Sibling](chapters/26-elixir.md) | Same BEAM, different vibe. |
| 27 | [Erlang Never Died](chapters/27-erlang-never-died.md) | Why this weird old language keeps winning. |

## License

This book is released under [CC0 1.0 Universal](LICENSE) — public domain. Do whatever you want with it. Translate it, remix it, print it and use it as a doorstop. We don't care. Just learn some Erlang.

## Contributing

Found a bug? Got a better joke? Know an Erlang fact too weird not to include? Open a PR. This book belongs to everyone.

{% raw %}
### A Note for Contributors: Erlang vs. Jekyll

This book is published via GitHub Pages using Jekyll. Jekyll's Liquid template engine uses `{{ }}` for variable interpolation — which collides with Erlang's match specifications, where `{{'$1', '$2'}}` is valid Erlang but looks like a Liquid tag.

If you're writing Erlang code that contains double curly braces (common in `ets:select`, `ets:match`, `mnesia:select`, and match specs generally), wrap the code block in Liquid raw/endraw tags:

````markdown
&#123;% raw %&#125;
```erlang
ets:select(people, [
    {{'$1', '$2', '$3'}, [{'>', '$3', 30}], ['$2']}
]).
```
&#123;% endraw %&#125;
````

Erlang's syntax crashes Jekyll. Fitting, really.
{% endraw %}

---

*Built with love, processes, and an unreasonable number of exclamation marks.*
