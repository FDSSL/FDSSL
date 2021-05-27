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
Each of these is a valid FDSSL program, but only the first two produce actual shaders. The last is more a test file. Currently, shaders will not run as expected, as we're still working on changing the pretty printer after introducing the parser.

## Structure

The structure of FDSSL is:
- `Examples`: various vertex & fragment shaders written in FDSSL, with a pair of example programs
- `Main`: easiest way to run and print shader programs
- `Pretty`: our pretty printer
- `Syntax`: Holds the abstract syntax for FDSSL
- `Parser`: Holds the parser for FDSSL
- `TypeChecker`: Empty, but will hold our type checker for programs written in FDSSL abstract syntax

## Milestone 2 (May 26th)

### Design Choices & Progress

- We have implemented a Parser using [parsec](https://hackage.haskell.org/package/parsec-3.1.14.0)
    - Through parsec, we have utiilzed parser-combinators, to create multiple separate parsers that when combined can parse a full FDSSL program. This has helped considerably with our concrete syntax
    - Parsec also provides a nice monad transformer stack, allowing us to use the State monad to great effect while parsing. Specifically, we can keep track of the uniforms, functions, and shaders that have been parsed along the way, allowing us to have all the proper references to create final programs.
- We sat down and tried to write what we thought would make good FDSSL programs. We have then used these programs as the basis to advise the parser itself. We initially sketched a formal specification, but found this was too abstract to determine what our requirements would be and what would look good in practice.
- We have rewritten parts of our syntax from before based on the program structure in our example programs. The result is a tad more imperative, but it has revealed limitations in our language that required further change.
- Composition of Shaders is implemented in such a way that it is *not* an expression, but a higher level construct only for building Shaders. Although odd at first, this choice works considering that shaders are not expressions in our language. The resulting shader is an abstract representation of the sequenced execution of both shaders, and also combines their effects. This behavior is not unlike the bind operation for the State monad.
- Shaders are written in our concrete syntax as functions that describe named inputs *and* named outputs (if any) along with an expression body. We believe this helps capture the idea that Shaders are functions with explicit side-effects that will impact the next shader down the line (if there is another shader).
- We are using Opaque Types in the abstract syntax of FDSSL to hide away the notion of uniform/attribute/varying qualifiers on types in GLSL. This helps keep the concrete syntax very simple type-wise, especially for those who are unfamiliar with GLSL.
- In the way we have written our parser, we have made it possible to express multiple shaders in the same program, as we planned in our initial proposal. However, we have also managed to make it so that multiple GLSL programs can be written from a single FDSSL program, giving further flexibility.

### What we're still working on

- We plan to write the resulting GLSL programs to output files, instead of the terminal output
- The shader output is currently broken from the last parser & syntax changes. The pretty printer needs to be changed to properly account for this.
- Using the example programs we have, a semi-formalization of the concrete syntax through the parser, and our old grammar, we would like to rewrite a new grammar to formalize the language specification as it is now.
- Composition of Functions works in the abstract syntax, but still needs to be expressible in the concrete syntax
- The parser needs to be refined to handle some edge cases
- Tests should be added to ensure the parser integrity is maintained
- The typechecker needs to be implemented to ensure integrity of the resulting shaders

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
