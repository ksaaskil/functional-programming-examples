---
title: Why I love learning functional programming
published: false
description: Yet another blog post on why learning functional programming is fun and useful
tags: functional,programming,haskell
series: Learning functional programming
---

This is the first part of the series on learning functional programming (FP). Inspired by [100 days of FP](https://dev.to/gillchristian/day-12-lambda-calculus-29hg), I'm also writing this series as my personal notes of my progress in learning FP. I do not claim to be an expert in FP, so if you think anything I write is inaccurate or plain wrong, please let me know in the comments!

In this first part, I'd like to share why I spend time on learning functional programming in the first place. Don't take me as a functional programming advocate or expert: at work, I mostly write imperative code and I'm a noob in writing real-world software in FP. However, I still spend time learning functional programming. This is why.

### It brings math to programming

The first reason I like functional programming is that to me, it brings math back to programming. At the university, I minored in match. I'll probably never have practical use to my courses in topology, differential geometry, measure theory or group theory, but I don't think any of those courses were waste of time. They all taught something about the power of abstraction, how to find and see the big concepts underlying seemingly unrelated problems.

In functional programming, you encounter abstractions like functors and monads all the time. Functional programming has roots deep in category theory, a branch of mathematics studying objects and their relationships. Category theory tells us, for example, that  [monad is just a monoid in the category of endofunctors](https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem). What the heck do those words even mean? I have no idea, but I need to find out!

I've been learning category theory from the wonderful [Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/) blog posts. They're are an easy and accessible way to access category theory. Maybe some day I'll be able to pick up a serious textbook on category theory! 

<!--In an [interesting talk](https://haskell.love/vitaly-bragilevsky/) on how to simplify learning Haskell, Vitaly Bragilevsky had the interesting point that there's actually nothing deep to understand in concepts like functors or monads. In Haskell, they're only type classes.-->

### It forces you to think differently

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

### FP is a logical conclusion to many ideas considered good programming style

My first touch to functional programming came from attending lectures in [functional programming](https://csd.cmu.edu/course-profiles/15-150-Principles-of-Functional-Programming) at CMU when I was a visiting researcher there. I attended maybe six lectures, where the lecturer wrote formal proofs that given recursive functions would terminate and have the correct result. It all seemed very theoretical to me and I thought I would not meet FP again.

However, I was introduced to FP soon in my working career as more experienced programmers [told me](https://dev.to/ksaaskil/how-i-accidentally-learned-functional-programming-1g1m) to avoid writing code with implicit side effects and mutable state where possible. Of course, I didn't really understand at the time that such ideas were built-in to FP.

As an example of how FP can help write clean code by avoiding implicit side effects, let's say you have a function

```ts
const containsFinnishLapphund: (jpegBase64: String) => boolean = ...
```

that checks if an image contains a [Finnish lapphund](https://www.youtube.com/watch?v=X0ejoDOmM6Q). The signature says the function takes a base64 encoded string and returns a boolean. Based on the signature, I _expect this function to not have implicit side effects_ like modifying the input or writing to a database. Therefore, I can safely call the function for 100 images in parallel without worrying about race conditions, deadlocks, or anything else.

The key here is the word _implicit_. In the context of my TypeScript codebase, I do not mind if the function prints to console: my code would most likely be interspersed with such logging statements anyway. However, I would be very surprised if calling the function incremented a database counter or stored the image to Google storage. Such surprises could lead to hard-to-find bugs, let alone they would make testing a pain.

In non-functional languages, it's the developer's responsibility to write code that is not surprising. In Haskell, however, a type signature such as

```hs
containsFinnishLapphund :: String -> Bool
``` 

would make it _impossible_ for the implementation to have observable side effects such as storing the image somewhere. If the function insisted on making a network call or logging to console, it would need a type signature

```hs
containsFinnishLapphund :: String -> IO Bool
```

The `IO` typeclass here makes it explicit that the function is doing _something_ with the external world. What does it do? For that, you'll need to read the code or trust the function docstring saying it doesn't do anything other than print to console. But at least, it's not a surprise anymore.

Another example of an "FP idea" considered good programming style nowadays is declarative style. For example, most programmers would nowadays agree that to remove even elements from an array and square the rest, this

```js
const double = (arr) => arr.filter(v => v % 2 === 0).map(v => v*v);
```

is preferred to this:

```js
const double = (arr) => {
    const newArr = []; 
    for (const i = 0; i++; i < arr.length) {
        if (arr[i] % 2 === 0) {
            newArr.push(arr[i] * arr[i]);
        }
    }
    return newArr;
}
```

In functional languages, the former would be the default. Again, this doesn't mean declarative style is better than imperative, but shows that declarative programming has its pros. In FP, the declarative style can be pushed to the limits with function composition operator `.` and point-free style:

```hs
square :: Int -> Int
square num = num * num

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

double :: [Int] -> [Int]
double = map square . filter isEven
```

I find this to be elegant and beautiful. It's true that imperative code can be easier to read and it takes time to get used to function composition and the point-free style, but I find it worth the effort.

### Conclusion

That concludes the first part of the series. I love learning functional programming because it gives me reason to read math again, is forces me to think differently, and it pushes the boundaries of good programming style. Thanks for reading, please leave a comment if you have any!

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