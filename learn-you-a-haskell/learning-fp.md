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
```

Haskell says that the inferred type of `True` is `Bool`, a boolean. `Bool` is defined in the [`Prelude`](https://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html) module imported by default.

Tuples are defined by parentheses:

```
ghci> :t (True, False)
(True, False) :: (Bool, Bool)
```

Function `fst` can be used to extract the first component from a pair (a tuple of two elements). In Haskell, function is invoked by putting the arguments after the function name, without parentheses:

```
ghci> myPair = (True, False)
ghci> fst myPair
True
```

What is the type of function `fst`?

```
ghci> :t fst
fst :: (a, b) -> a
```

Here, `a` and `b` are so-called _type variables_. They represent any type, a bit like _generics_ in languages such as Java.

As another example of function, consider `&&`, the `AND` operator familiar from most programming languages. When you write the following in Haskell, 

```
ghci> True && False
False
```

you're actually evaluating the function `(&&)` with two arguments, `True` and `False`. In Haskell, functions consisting of only special characters are considered _infix_ functions by default, meaning that the function should be sandwiched between its arguments. The parentheses are required for examining the type or calling it as a prefix function: 

```

ghci> (&&) True False
False
```

Parentheses are also required if calling the functions as a prefix ("normal") function:

```
ghci> :t (&&)
(&&) :: Bool -> Bool -> Bool
```

## Typeclasses

What is the type of `2`? Let's see:

```
ghci> :t 2
2 :: Num p => p
```

What does this mean? Haskell says that the inferred type of `2` is `p`, where `p` is a so-called _type variable_. A type variable represents any type satisfying the _class constraints_ on the left side of `=>`. In this case, the class constraint is  `Num p`, meaning that the type `p` belongs to the _typeclass_ `Num`.

Here's another example:

```
ghci> :t [1, 2, 3]
[1, 2, 3] :: Num a => [a]
```

Now, Haskell says that the inferred type of `[1, 2, 3]` is a list of type variable `a`, where `a` again has the type constrain that it belongs to the `Num`.  

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