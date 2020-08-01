---
title: Why I love learning functional programming
published: false
description: Yet another blog post on why learning functional programming is fun and useful
tags: functional,programming,haskell
series: Learning functional programming
---

This is the first part of the series on learning functional programming (FP). Inspired by [100 days of FP](https://dev.to/gillchristian/day-12-lambda-calculus-29hg), I'm also writing this series as my personal notes of my progress in learning FP. I do not claim to be an expert in FP, so if you think anything I write is inaccurate or plain wrong, please let me know in the comments!

In this first part, I'd like to share why I spend time on learning functional programming in the first place. Don't take me as a functional programming advocate or expert: at work, I mostly write imperative code and I'm a noob in writing real-world software in FP. However, I still spend time learning functional programming. This is why.

### 1. It brings math to programming

The first reason I like functional programming is that to me, it brings math back to programming. At the university, I minored in match. I'll probably never have practical use to my courses in topology, differential geometry, measure theory or group theory, but I don't think any of those courses were waste of time. They all taught something about the power of abstraction, how to find and see the big concepts underlying seemingly unrelated problems.

In functional programming, you encounter abstractions like functors and monads all the time. Functional programming has roots deep in category theory, a branch of mathematics studying objects and their relationships. Category theory tells us, for example, that  [monad is just a monoid in the category of endofunctors](https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem). What the heck do those words even mean? I have no idea, but I need to find out!

I've been learning category theory from the wonderful [Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/) blog posts. They're are an easy and accessible way to access category theory. Maybe some day I'll be able to pick up a serious textbook on category theory! 

<!--In an [interesting talk](https://haskell.love/vitaly-bragilevsky/) on how to simplify learning Haskell, Vitaly Bragilevsky had the interesting point that there's actually nothing deep to understand in concepts like functors or monads. In Haskell, they're only type classes.-->

### 2. It teaches you to think differently

Putting aside playing with [Basic](https://en.wikipedia.org/wiki/BASIC) in the 90s, I first learned programming at the university in Java and C. In those languages, programs are written using constructs such as if-clauses and for-loops. Data is typically modified in-place with functions or method calls that return nothing.

If-clauses, for-loops and in-place mutations are easy to understand, because that's how we intuitively process data. If I'm given a list of `N` skills that I need to learn unless I already know the skill, here's what to do:

0. Set `i=1`
1. Take the `i`th skill from the list
2. Check if you know the skill. If you don't, learn the skill.
3. If `i=N`, exit. Otherwise, set `i = i+1` and go to `1`.

This is an imperative program, with one command after another that modify the program state. This is intuitive to us, because that's how we process data in everyday life. To us, world is made of mutable objects. That's how computers also work, one statement after another modifying the program state. 

Now, imagine you're told you need to write code for a program without a single if-clause or for-loop. You are also forbidden to mutate objects. What you're allowed to do is create new objects and write _pure_, _referentially transparent_ functions. Referential transparency means that a function call can be replaced by its return value without any change in the program. So for example, this function is not referentially transparent:

```python
def square(x):
    print(f"Computing the square of {x}") 
    return x*x
```

because you can't replace `square(x)` with `x*x` and expect the program to remain unchanged.

It goes without saying that such constraints force you to think differently about writing code. To me, that's a very good thing. Recently I've been writing code mostly in Python and JavaScript. While I love both languages for their flexibility and simple syntax, and there's always something new to learn in both of them, I don't think they offer that many chances for learning new _concepts_. I write the same if-clauses and for-loops day after another, possibly in some new framework, but at the end of the day, the programs look the same.

With functional programming, programs will inevitably look different. Are they better? That's an ill-posed question, as there's no best code for a particular task. But I do think it's useful to learn different ways of writing code. The more tools you have at your disposal, the more likely it is that you can pick the best one for the job when new problems emerge.  

Now, a fact is that my employer most likely wouldn't appreciate me spending the whole morning figuring out how to [make a HTTP call](https://dev.to/ksaaskil/using-fp-ts-for-http-requests-and-validation-131c) or spending the morning explaing my colleagues how data type `Maybe` replaces `if`. That's why learning FP is mostly a hobby to me at the moment. However, if I had colleagues at work as enthusiastic about FP as me, situation would get different. Everyone could support each other in learning how to solve problems in a functional way, knowledge would spread through the whole team in the form of peer reviews and code, and the cost of learning new concepts would be lower as those new concepts might improve everybody's code base.

Finally, I'd like to note that functional programming doesn't mean you're not programming imperatively. Here's one excerpt of Scala code from the [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala) book ("red book"):

```scala
val factorialREPL: IO[Unit] = sequence_(
    IO { println(helpstring) },
    doWhile { IO { readline } } { line =>
        when (line != "q") {
            for {
                n <- factorial(line.toInt)
                _ <- IO { println("factorial: " + n) }
            }
        } yield ()
    }
)
```

That's a purely functional program written in imperative fashion. The for-loop is Scala's [syntactic sugar](https://docs.scala-lang.org/tutorials/FAQ/yield.html) for the composition of operations such as `map`, `filter` and `flatMap`.

### 3. FP is good programming style

I think functional programming is the logical conclusion to many ideas considered good programming style. Avoiding side effects, favoring immutable objects, and using composition instead of, for example, inheritance to achieve code re-use are principles built-in to functional programming. Don't get me wrong: at work I still write functions with side effects (logging, analytics events, etc.) or use mutable objects (for, e.g., complex data structures). In my experience, however, favoring functional code typically results in easier-to-read and modular code. It's interesting to learn how far you can push such ideas.


---
title: Why I love learning functional programming
published: false
description: Yet another blog post on why I think learning FP is fun and useful
tags: functional,programming,haskell
series: Learning functional programming
---

## Why Haskell?

### 1. Haskell is lingua franca

### 2. Haskell is purely functional

### 3. Haskell has a powerful type system



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