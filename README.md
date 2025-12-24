# Animator

This package is a fairly general collection of tools to work with functions of the form `Float -> a`, where the `Float` is generally assumed to be time.

I encounter this kind of function frequently when making animations, and this is what this is specifically geared towards doing.

## Generality and abstraction

This package breaks a little from some Elm philosophy by actually being decently abstract. Notably, it's designed to be almost entirely agnostic as to what its output shape is, and so is limited with what it can do on the output. I think this is generally good and helpful. It however does mean that it's useful to specialize some functions (notably, `alongWith`) for your specific use case.


## Examples

As of Dec 2025, I don't yet have examples that are useful, but hopefully will have more to show soon as I publish more of my tools.
