# Functional Domain Specific Shader Language

FDSSL is a functional language that is translated into the OpenGL Shader Language (GLSL).
The goal of the language is to simplify the implementation of Shaders in order to make
shader programming more approachable, with the added feature of making Shaders composeable.

We are unsure as of yet as to whether or not it will be a Haskell DSL or whether or not
it will have a concrete syntax. We also intend to incorporate the Vertex and Matrix
types to allow for matricies to be computed by haskell prior to being used in variable
declarations.

How can we improve the `Func` data type? At the moment we use it as either a Function or a
variable to be passed into the shader. This is so the variables fit easier into the environment
but I feel like this could use some improvements.

How can we handle Shader composition? Generally speaking each shader has a variable that
it is required to set in order to pass data to the next shader. If the transformation of
values and the result the variable is assigned to are completely different, what should
we do?


# Instructions
This project uses Cabal. Running this program results in a simple shader program
(Vertex + Fragment) outputted to STDOUT.

You can do this by runing `cabal run`.


    A brief description of your project goals (you can take this from your project proposal), and your current progress toward achieving those goals.
    Instructions for how to run your project (e.g. which file to load in GHCi), including several example inputs, if applicable.
    In Milestone #1: a list of 2–4 design questions that you have about your project, that you would like to discuss during the workshop.

