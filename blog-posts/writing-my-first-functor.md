---
title: Writing my first functor
published: false
description: What is a functor exactly and how do I use it?
tags: scala, typescript fp
# canonical_url: link for the canonical version of the content
# cover_image: cover image for post, accepts a URL. 
The best size is 1000 x 420.
# series: post series name.
---

When learning functional programming, sooner or later one encounters new mathematical concepts like functor.
Googling for "what is a functor" you find explanations ranging [from](https://medium.com/@dtinth/what-is-a-functor-dcf510b098b6) "something you can map over" [to](https://en.wikipedia.org/wiki/Functor) "a map between categories".

Here, I attempt to summarize my learnings and bridge the gap between these definitions. I provide the definitions required for applying functors in daily programming. The code examples will be in Scala and TypeScript with some Haskell thrown in, but the actual language should not matter that much.

## Function composition

According to Giulio Canti, functional programming is all about function composition, i.e. applying functions after one another. Composing functions of types `f: A => B` and `g: C => D` is trivial when the types `B` and `C` match, but what can you do if `B` and `C` are not the same? That's where concepts such as functor come in.

For example, let us assume you have the following functions:
```typescript
const fetchUsers: () => List<User> = ...
const getAddress: (user: User) => Address = ...
```
You want to compose the two functions to fetch the list of all user addresses. How do you compose `fetchUsers` and `getAddress` to produce a `List<Address>`? 

Obviously, you want to apply the `map` function. This is possible, because one can define a *functor* for `List`. Functor is indeed something you can "map over", but it must also obey specific laws. For example, we expect that `map` with an identity function keeps the list intact.

Before we get to defining our own functors, we need to talk about types to understand what types can "have a functor". It is always useful to start from type signatures before diving into implementation.

## Types, proper types, type constructors, and all that

### Abstracting over types

- There are proper types like `String` or `Number`. You can instantiate these. In Haskell, the type would be `*`.
- There are type constructors. For example, `List` is a type constructor. You apply the type constructor (`List`) to a proper type (`String`) to produce a proper type (`List[String]`). Type constructor abstracts over its input. In Haskell, the type would be `* -> *`.
- But why stop here, surely we can define something that abstracts over something that abstracts over something, i.e. have something of the type `(* -> * ) -> *`? We arrive at "higher-order" types, or "higher-kinded" types.
- In Scala: 
```scala
trait Iterable[A, Collection[_]] {
    def filter((a: A) => Boolean): Collection[A]
}
```
Here `Iterable` consumes a proper type `A` as well as a type constructor `Collection`. We can therefore define `Iterable` behaviour for `List`, `Set`, or whichever type constructor for which it makes sense.

### Functor is a type constructor consuming type constructors

- Functor is a higher-kinded type in that it consumes type constructors, not proper types. You cannot define a functor for String, but you can define a functor `Functor[List]`. 
- Note that `Functor[List]` is not a type constructor waiting for input such as `Number`: it is a proper type because you can instantiate it.
- Note that higher-kinded types are also type constructors. Therefore, we should revise our earlier definition of type constructor to not only accept proper types but also other type constructors.


```scala
val a = 1;

trait Functor[F[_]] {
    map(f: (a: A) => B)(fa: F[A]) => F[B]
}
```

## Functor laws
Functor is not only something you can map over: it must also obey specific laws to actually make it a useful abstraction.

## Concrete examples

## Resources

- [What is a higher kinded type in Scala](https://stackoverflow.com/questions/6246719/what-is-a-higher-kinded-type-in-scala)
- [What is a functor?](https://medium.com/@dtinth/what-is-a-functor-dcf510b098b6) (comment section contains excellent discussion on what's not a functor)
- See [Higher kinded types in TypeScript](https://medium.com/@gcanti/higher-kinded-types-in-typescript-static-and-fantasy-land-d41c361d0dbe) for how to implement higher kinded types in TypeScript (or just use [fp-ts](https://github.com/gcanti/fp-ts))