<br>
<p align=center><img src="clover.png" width=150vw></p>

# The Clover Programming Language

```py
fun greet():
    print("Hello from Clover!")
```

## Table of Contents

### [1. About](#1-about)

### [2. Installation](#2-installation)

### [3. Roadmap](#3-roadmap)

 * **[3.1. Language Features](#3-1-language-features)**
 * **[3.2. Platform Support](#3-2-platform-support)**

### [4. Tour](#4-tour)

## 1. About

### What is Clover, anyway?

Clover is a low-level procedural programming language intended to function as an alternative (not a replacement!) to C for low-level programming. Overall, Clover is designed to incorporate more recently-popular and modern language features into a simple, procedural core, leading to code that is graceful, aesthetically-pleasing, and intuitive.

Here's a non-exhaustive list of some of my core design principles so you can get a better idea of where my priorities lie (a "Zen of Clover" if you will):

1. Making code pretty is worth it!
2. ...but abstraction can't come at the cost of performance.
3. Behavior shouldn't be undefined.
4. Types should make life easier, not harder.
5. The safest option is the best default.
6. Overlapping language features lead to confusion.
7. Data should be what you think it is.
8. Code should do what you think it does.
9. Batteries aren't included.
10. ...but they should be close by when you need them.

### But...why Clover?

There's loads of other fancy, procedural, prospective C replacements out there, [to](https://ziglang.org/) [name](https://odin-lang.org/) [just](https://nim-lang.org/) [a](https://inductive.no/jai/) [few](https://vlang.io/). Why does Clover need to exist too?

Mostly I just felt like it! Personally, I'm mostly a C++ person, but after writing yet another header file working on my [other language project](https://github.com/basilTeam/basil/) I decided there had to be a better way. Where in the past I've explored PL from the perspective of making a novel high-level language, Clover is moreso just a compilation of all the indulgent syntactic sugar I want in a C-like that I haven't seen elsewhere.

As a result, there's not a ton of real novel stuff I can advertise that Clover does _uniquely_ well! It's intentionally not very novel. So I guess if you want to be sold on the project, I don't have a lot of big-ticket items to show off. Sorry!

What I _do_ think Clover manages is a particularly consistent and pleasant design that I haven't seen in any of the other languages I linked above (Nim probably comes the closest). Since starting the Clover project, I often find myself using it as my go-to pseudocode syntax, where before I would usually use something more Pythonic. And I also think it's pretty fun to use, although the early state of the language means I haven't written anything too gigantic in it yet.

There are also two other major experimental aspects inherently tied to Clover that are tangential to the language itself:

 * All of Clover runs on **libcore**, a standalone native runtime that implements a barebones OS interface on different platforms. This means Clover programs are pretty much freestanding - you can statically link them on any platform where libcore can be statically linked, with no need to depend on libc. This also includes the Clover compiler itself, even though it's written in C++.

 * Clover's compiler also aims for especially high compiler throughput. The compiler internals are highly optimized, with the aim of compiling Clover source to any of the available backends at over a million lines per second. That's the goal, anyway - while the current compiler should be pretty darn fast, more optimizations are needed as well as some good benchmarks to measure compiler performance.

## 2. Installation

Currently, if you want to install or contribute to Clover, you need to build it from source. In order to do this, you will need:

 * A C++ compiler (with support for at least C++11)
 * GNU Make
 * Git

On one of the [supported platforms](#3-2-platform-support), run the following:

```
$ git clone --recurse-submodules https://github.com/elucent/clover
$ make clover                                   # Compiles using clang++ by default. You can
$ ls bin                                        # set CXX to a custom C++ compiler if needed.
  clover libcclover.a 
$ bin/clover --version
Clover version 0.1.0 for amd64 linux            # Version numbers and target may differ!
```

To add the `clover` compiler to your system PATH, you can run the following (on Linux, using `bash` shell):

```
$ echo PATH=$PATH:$(pwd)/bin >> ~/.bashrc
$ source ~/.bashrc
$ clover --version
Clover version 0.1.0 for amd64 linux
```

To run all the unit tests, run:

```
$ mdtest/mdtest test
```

## 3. Roadmap

### 3.1. Language Features

- [x] Primitive types, arithmetic, comparisons, logic, etc.
- [x] Variables.
- [x] First-class functions.
- [x] Pointer, array, and slice types.
- [x] Method calls using UFCS.
- [x] Control flow: `if`, `while`, `until`
- [x] For loops, iteration.
- [x] Modules, `use` statements, `with` blocks.
- [x] Type inference using unification variables.
- [x] Type aliases.
- [x] Struct, union, and named types.
- [x] Generic function and type definitions.
- [x] Named type variables.
- [x] Manual memory management.
- [x] Pattern matching.
- [x] Module initialization and deinitialization.
- [x] `defer` statements.
- [x] Recoverable compile errors with good messages.
- [ ] Traits.
- [ ] Compile-time constant values and expressions.
- [ ] Set and dictionary constructors.
- [ ] List, set, and dictionary comprehensions.
- [ ] Anonymous functions.

### 3.2. Platform Support

- [x] Linux AMD64
- [x] Darwin AMD64
- [ ] Windows AMD64
- [ ] Linux ARM64
- [ ] Darwin ARM64
- [ ] Windows ARM64
- [ ] WASM

### 3.3. Compiler Backends

- [x] C99
- [ ] LLVM
- [ ] WASM
- [ ] Jasmine

## 4. Documentation

Coming soon!
