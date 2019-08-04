---
title: What is functional programming
published: false
description: My attempt to summarize what functional programming could be
tags: scala, typescript fp
# canonical_url: link for the canonical version of the content
# cover_image: cover image for post, accepts a URL. 
The best size is 1000 x 420.
# series: post series name.
---

- I'm not an expert and I do not know category theory

## It is about programming without side effects

- Referential transparency
- Pure functions
- Side effects with `IO`
- Immutable objects

## It is about function composition

- Compose functions `f: A => B` and `g: C => D` in a useful way
- Functors, monads, traverse, and all that are just the language for composing functions

## It is not the opposite of imperative programming

```scala
for (
    x <- myFunction
    y <- myFunction2(x)
) yield ( 2 * y)
```

## It does not need to replace object-oriented programming

- You can still write object-oriented programs but apply the principles of functional programming for easier testability, better readability, reducing the mental burden
- Functional programming can produce unreadable code

## It is a lot of fun

- Discovering abstractions is fun, just as learning design patterns for OOP programs is fun
