# Functional Domain Specific Shader Language

## Introduction

FDSSL is a domain specific functional language that is translated into the OpenGL Shader Language (GLSL).
The goal of the language is to simplify the implementation of Shaders in order to make
shader programming more approachable, with the added feature of making Shaders composable.

At a high level, we describe GLSL programs with the following semantic domain.
```
Env -> Env -> (Env,Expr) -> Env
```

Where an `Env` is equivalent to a list of named functions.

More explicitly, we can write this as:
```
GlobalEnv -> PriorResultEnv -> (ShaderEnv,Expr) -> ShaderResultEnv
```
To explain this, a global environment is shared across all shaders. Each shader also gets results that are passed to it from a prior stage, which is produced by some prior step that is not known in advance. Finally, every shader has its own environment and an expression to evaluation in that environment. The result of evaluating this shader produces a result environment, which is then fed into the next stage (if there is a following stage).

## Instructions to Run

This project uses Stack now. Running this program results in a simple shader program
(Vertex + Fragment) outputted to STDOUT. To run these example shader programs, we've written up a simple webpage to test them in, [https://www.uphouseworks.com/fdssl-test.html](https://www.uphouseworks.com/fdssl-test.html).

To run with stack, you can use:
```
stack run
```
Which will build and run the executable, and output two resulting shaders (a vertex and a fragment shader).

To dynamically run it, you can run the following in your terminal
```
stack ghci
```
once that's finished, you can evaluate some FDSSL programs like so
```
> evalProg "examples/e0.txt"
> evalProg "examples/e1.txt"
> evalProg "examples/e2.txt"
```
Each of these is a valid FDSSL program, but only the first two produce actual shaders. The last is more a test file.

## Structure

The structure of FDSSL is:
- `Examples`: various vertex & fragment shaders written in FDSSL, with a pair of example programs
- `Main`: easiest way to run and print shader programs
- `Pretty`: our pretty printer
- `Syntax`: Holds the abstract syntax for FDSSL
- `Parser`: Holds the parser for FDSSL
- `TypeChecker`: Empty, but will hold our type checker for programs written in FDSSL abstract syntax

## Milestone 2 (May 26th)

### Progress

- We have implemented a Parser using [parsec](https://hackage.haskell.org/package/parsec-3.1.14.0)
    - Through parsec, we have utiilzed parser-combinators, to create multiple separate parsers that when combined can parse a full FDSSL program. This has helped considerably with our concrete syntax
    - Parsec also provides a nice monad transformer stack, allowing us to use the State monad to great effect while parsing. Specifically, we can keep track of the uniforms, functions, and shaders that have been parsed along the way, allowing us to have all the proper references to create final programs.
- We sat down and tried to write what we thought would make good FDSSL programs. We have then used these programs as the basis to advise the parser itself. We initially sketched a formal specification, but found this was too abstract to determine what our requirements would be and what would look good in practice.

### What we're still working on

- Using the example programs we have, a semi-formalization of the concrete syntax through the parser, and our old grammar, we would like to rewrite a new grammar to formalize the language specification as it is now.

### Questions

...

## Milestone 1 (May 12th)

## Progress

- We have written up an abstract syntax for FDSSL
- We have decided to *not* make this a pure functional language, so functions do have effects
  - we did this because in GLSL, the sole purpose of some functions is the effect they produce on the environment
- We have written up a `Shader` data type, and defined a way to compose shaders. Since functions are effectful, composition is an effective sequencing of the first shader followed by the second.
- We wrote a deep embedding for FDSSL, with a way to 'lift' values from Haskell into the domain of FDSSL via a `Wrappable` typeclass
- We have defined some example shaders that demonstrate how this works, with composition in place

## What we're still working on

- Because we're using a deep embedding, we still need a typechecker
-Our current implementation is a Haskell DSL, but we are unsure as to whether or not
FDSSL will have a concrete syntax
- We intend to incorporate the native Vertex and Matrix types in Haskell to allow for matrices to be computed in Haskell prior to being used in FDSSL to produce a vertex or matrix in a corresponding GLSL program. This is roughly in place, but not correctly done yet
- Function composition is not done correctly yet, as functions are currently composable without regard to their types
- The abstract syntax works, but we would like to refine

## Questions

#### How can we improve the `Func` data type?
At the moment we use it as either a Function or a
variable to be passed into the shader. This is so the variables fit easier into the environment
but I feel like this could use some improvements.

#### How can we improve Shader composition?
Generally speaking each shader has a variable that
it is required to set in order to pass data to the next shader. If the transformation of
values and the result of the variable it is assigned to are  different, what should
we do? We're thinking that maybe we should simply fail and produce no shader.

#### How can we improve using Func for actual functions *and* global values
Functions, as defined in our context via `Func`, are used generally to define all items in the environments. This means some functions have special modifiers if they are passed in the 'global' position. If we do this, should the functions not handle those modifiers themselves? Or should we consider some other way to clean this up?

#### We're thinking of using Monads to help sequence composition, but...
...our shaders are not parameterized by any type. We're thinking maybe we can change this, or that we can use the example Eric brought up in class about 'describing a plan' for a computation, rather than performing the computation itself. We think we can use our semantic domain to achieve this, or some form of it, but we're open to feedback about how we might go about it. Also, we're thinking this might be helpful for functions, since they are effectful as well.
