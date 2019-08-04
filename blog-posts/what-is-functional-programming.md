---
title: What is functional programming
published: false
description: What is a functor exactly and how do I use it?
tags: scala, typescript fp
# canonical_url: link for the canonical version of the content
# cover_image: cover image for post, accepts a URL. 
The best size is 1000 x 420.
# series: post series name.
---


## It is about programming without side effects

- Referential transparency
- Pure functions
- Side effects with `IO`
- Immutable objects

## It is about function composition

- Compose functions `f: A => B` and `g: C => D` in a useful way
- Functors, monads, traverse, and all that are just the language for composing functions

## It is NOT the opposite of imperative programming

```scala
for (
    x <- myFunction
    y <- myFunction2(x)
) yield ( 2 * y)
```