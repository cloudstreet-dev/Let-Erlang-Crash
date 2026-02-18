# Chapter 2: Installing Erlang (The Easy Part)

> *The hardest part of learning Erlang is not the syntax, the concurrency model, or the lack of for loops. It's getting it installed without accidentally building it from source and staring at a terminal for 45 minutes. Let's skip that.*

---

## The Goal

We need two things on your machine:

1. **Erlang/OTP** — the runtime, compiler, and standard library
2. **rebar3** — the build tool (think npm, cargo, or mix, but for Erlang)

That's it. No IDE plugins required. No complicated toolchains. Erlang is refreshingly low-ceremony.

## macOS

If you're on a Mac, Homebrew makes this painless:

```bash
brew install erlang rebar3
```

Done. Seriously. Go get a coffee while it compiles (Erlang is a big boy).

Verify it worked:

```bash
erl -version
```

You should see something like:

```
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 14.x
```

## Linux (Ubuntu/Debian)

The distro packages are often ancient. Use the Erlang Solutions repo instead:

```bash
# Add the Erlang Solutions repo
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update

# Install Erlang and rebar3
sudo apt-get install -y erlang rebar3
```

Or if you're a cool kid who uses `asdf`:

```bash
asdf plugin add erlang
asdf install erlang latest
asdf global erlang latest

asdf plugin add rebar
asdf install rebar latest
asdf global rebar latest
```

## Linux (Fedora/RHEL)

```bash
sudo dnf install erlang rebar3
```

If the version is too old, use `asdf` or `kerl` (see below).

## Windows

Erlang on Windows works fine, despite what the Unix nerds will tell you.

1. Download the installer from [erlang.org/downloads](https://www.erlang.org/downloads)
2. Run the `.exe` installer
3. Make sure `erl` is on your PATH
4. For rebar3, grab the Windows release from [rebar3.org](https://rebar3.org)

Or use WSL2 and follow the Linux instructions, which is what most people actually do.

## Using kerl (The Power User Way)

`kerl` is like `nvm` but for Erlang. It lets you install and manage multiple Erlang versions:

```bash
# Install kerl
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod a+x kerl
sudo mv kerl /usr/local/bin/

# List available releases
kerl list releases

# Build and install a specific version
kerl build 27.0 27.0
kerl install 27.0 ~/kerl/27.0

# Activate it
. ~/kerl/27.0/activate
```

## Using asdf (The Polyglot Way)

If you already use `asdf` for managing language versions (and you should), Erlang fits right in:

```bash
asdf plugin add erlang
asdf list all erlang    # See what's available
asdf install erlang 27.0
asdf global erlang 27.0
```

Note: building Erlang from source via asdf can take a while. This is normal. The BEAM is a substantial piece of engineering.

## Docker (The "I Don't Want to Install Things" Way)

```bash
docker run -it erlang:27 erl
```

Boom. You're in an Erlang shell. No installation required.

## Verify Everything Works

Open a terminal and type:

```bash
erl
```

You should see something beautiful like this:

```
Erlang/OTP 27 [erts-14.0] [source] [64-bit] [smp:8:8] [ds:8:8:10]
              [async-threads:1] [jit:ns]

Eshell V14.0 (press Ctrl+G to quit)
1>
```

That `1>` prompt is the Erlang shell. It's waiting for you. Type something:

```erlang
1> 2 + 2.
4
2> "hello".
"hello"
3> erlang:system_info(otp_release).
"27"
```

**Important:** Note the period at the end of each expression. In Erlang, a period (`.`) terminates an expression. Forget it and the shell will stare at you patiently forever. We've all been there.

## Your Editor

Erlang doesn't need a fancy IDE. Any text editor works. But if you want nice things:

- **VS Code** — Install the "Erlang/OTP" extension by PGoetz or the "Erlang LS" extension
- **Emacs** — erlang-mode ships with Erlang/OTP itself
- **Vim/Neovim** — vim-erlang plugins or Tree-sitter grammars
- **IntelliJ** — The Erlang plugin works reasonably well

The Erlang community has a long tradition of using Emacs, because of course it does. Use whatever makes you happy.

## Your First Erlang File

Create a file called `hello.erl`:

```erlang
-module(hello).
-export([world/0]).

world() ->
    io:format("Hello, BEAM world!~n").
```

Compile and run it from the shell:

```erlang
1> c(hello).
{ok,hello}
2> hello:world().
Hello, BEAM world!
ok
```

If that worked, you're ready. Your machine has a functioning BEAM runtime, and you just ran your first Erlang module.

## What We Just Did

- Installed Erlang/OTP (the language runtime)
- Verified the shell works
- Created and ran a module

The boring part is over. Let's go explore the shell.

---

[← Previous: Why Erlang?](01-why-erlang.md) | [Next: The Shell →](03-the-shell.md)
