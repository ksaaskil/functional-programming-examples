---
title: What are kinds in functional programming?
published: false
description: Understanding proper types, type constructors, and higher kinded types
tags: functional,programming,haskell
series: Learning functional programming
---

This is the first part of the series on learning functional programming (FP). Inspired by [100 days of FP](https://dev.to/gillchristian/day-12-lambda-calculus-29hg), I'm also writing this series as my personal notes of my progress in learning FP. I do not claim to be an expert in FP, so if you think anything I write is inaccurate or plain wrong, please let me know in the comments!

In this first part, I'd like to speak about types. I'll use [Haskell](https://www.haskell.org/) in the code snippets below. While I've never used Haskell in my working career in software development (and probably won't), I think Haskell is the best language to learn functional programming concepts. Being a pure functional language, Haskell won't let you "cheat" by reverting to imperative programming. There are also wonderful resources for learning Haskell such as the [Learn You a Haskell](http://learnyouahaskell.com/introduction) book available for free.

I'll use the [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html), the interactive environment of the [GHC](https://www.haskell.org/ghc/) Haskell compiler to explore types. If you'd like to follow along, you can install Haskell with [Stack](https://docs.haskellstack.org/en/stable/README/) and enter

```bash
$ stack ghci
```

to enter the `ghci` REPL.

## Types of values

In programming, we typically deal with two kinds of objects: values and types. In Haskell interpreter, we can use `:t` to print types of values as follows:

```
ghci> :t True
True :: Bool
ghci> :t [True, False, True]
[True, False, True] :: [Bool]
```

These expressions show that the type of `True` is `Bool`, boolean. The type of `[True, False, True]` is `[Bool]`, i.e., a list of booleans.

So far so good. Let's play a bit more:

```
ghci> :t [1, 2, 3]
[1, 2, 3] :: Num a => [a]
```

What does this mean? Here, Haskell says that the inferred type of `[1, 2, 3]` is `[a]`, a list of `a`. Here `a` is a so-called _type variable_, any type satisfying the class constraint `Num a`. This means that the type `a` must belong to the `Num` typeclass.  

## Kind

I'll cite following on the Wikipedia article on [kinds](https://en.wikipedia.org/wiki/Kind_%28type_theory%29):

> In the area of mathematical logic and computer science known as type theory, a kind is the type of a type constructor or, less commonly, the type of a higher-order type operator.

```ghci
ghci> :k []
[] :: * -> *
```

```
ghci> :k Maybe
Maybe :: * -> *
```

```ghci
ghci> :k Functor
Functor :: (* -> *) -> Constraint
```


## Resources

- [What is a higher kinded type in Scala?](https://stackoverflow.com/questions/6246719/what-is-a-higher-kinded-type-in-scala)
- [Generics of a Higher Kind](https://adriaanm.github.io/files/higher.pdf)
- [Kind (type theory)](https://en.wikipedia.org/wiki/Kind_%28type_theory%29) 
- [Type Constructor Polymorphism](https://adriaanm.github.io/research/2010/10/06/new-in-scala-2.8-type-constructor-inference/)
- [Learn You a Haskell](http://learnyouahaskell.com/)
- [What exactly is the kind in Haskell](https://stackoverflow.com/questions/27095011/what-exactly-is-the-kind-in-haskell)