---
title: Introduction to composable optics with monocle-ts
published: false
description: With some io-ts
tags: typescript, optics, functional programming
series: Introduction to monocle-ts
---

Optics are a tool employed in functional programming to zoom into nested data structures. They are designed for composability, allowing you to create complex operations step-by-step by composing simple components. Optics also never modify their input data structures, ensuring your objects stay nice and immutable.

Giulio Canti's [monocle-ts](https://github.com/gcanti/monocle-ts) library is a TypeScript port of Scala's [Monocle](https://julien-truffaut.github.io/Monocle/) library, which in turn is inspired by Haskell's [Lens](https://hackage.haskell.org/package/lens) library. It provides "a highly generic toolbox for composing families of getters, folds, isomorphisms, traversals, setters and lenses and their indexed variants."

In this first article, we'll use two optics: lenses and optionals. In the next article, we'll dive deeper into traversals, isomorphisms, and prisms.

We'll use the [io-ts](https://github.com/gcanti/io-ts) library for defining the types for our examples. Using `io-ts` is a small detour, but I think it's useful to understand how it can work together with `monocle-ts`.

The code for this tutorial can be found from [monocle-ts/](https://github.com/ksaaskil/functional-programming-examples/tree/master/monocle-ts) folder of [this repository](https://github.com/ksaaskil/functional-programming-examples). In the repository you can also find Jest tests showcasing all the functionality presented in this article.

## Getting started with `io-ts`

`io-ts` is a run-time type system. It allows you to add run-time type-checking to those pesky `Any` objects you get from external sources like user inputs, files, or databases. Let's consider a simple `Hobby` interface defined as follows:

```ts
interface HobbyI {
  name: string;
}
```

The way to define this in `io-ts` is as follows:

```ts
import * as t from "io-ts";
const HobbyT = t.interface({ name: t.string });
type Hobby = t.TypeOf<typeof HobbyT>; // Static type
```

I use the `T` extension to mark `io-ts` types. It's important to notice that the `HobbyT` is a `const` and not a type: it's an object that remembers its properties even after the `TypeScript` code is transpiled to JavaScript. Therefore, one can use the `HobbyT` object at _run-time_ to check if objects are actually valid hobbies or not.

`Hobby`, on the other hand, is a static type equivalent to `type Hobby = { name: string }`. `Hobby` is a pure TypeScript thing and does not exist anymore after transpilation.

`HobbyT` has an `is` method you can use to check if objects are valid hobbies:

```ts
it("accepts an valid hobby object as HobbyT", () => {
  const isHobby = HobbyT.is({ name: "Photographing corgis" });
  expect(isHobby).toBe(true);
});
it("does not accept an invalid hobby object as HobbyT", () => {
  const isHobby = HobbyT.is({ name: 66 });
  expect(isHobby).toBe(false);
});
```

For more stringent validation and error messages, you can use `decode`:

```ts
import { isLeft, isRight } from "fp-ts/lib/Either";

it("can decode a hobby from valid input", () => {
  const maybeHobby = HobbyT.decode({ name: "Petting corgis" });
  expect(isRight(maybeHobby)).toBe(true);
});
it("does not decode a hobby from invalid input", () => {
  const maybeHobby = HobbyT.decode({ name: 67 });
  expect(isLeft(maybeHobby)).toBe(true);
});
```

`decode` method returns an [Either](https://gcanti.github.io/fp-ts/modules/Either.ts.html) object, whose value can be "left" or "right" corresponding to either failure or success, respectively. If there's an error, the either contains a "left" of `t.Errors` type defined as follows:

```ts
export interface Errors extends Array<ValidationError> {}
```

Validation errors can be printed with, for example, the [PathReporter](https://github.com/gcanti/io-ts#error-reporters) utility. You can read more about the `Either` type in my [previous article on `fp-ts`](https://dev.to/ksaaskil/using-fp-ts-for-http-requests-and-validation-131c).

Here are the rest of the types we'll need:

```ts
const PersonT = t.interface({
  firstName: t.string,
  age: t.number,
  hobbies: t.array(HobbyT),
});
type Person = t.TypeOf<typeof PersonT>;

const BandT = t.interface({ name: t.string, members: t.array(PersonT) });
type Band = t.TypeOf<typeof BandT>;

// type Artist = Person | Band;
const ArtistT = t.union([PersonT, BandT]);
type Artist = t.TypeOf<typeof ArtistT>;
```

`Person` is an object with `firstName`, `age` and an array of hobbies. A band is an object with `name` and `members`, where `members` is a list of persons. Finally, `Artist` is defined as being either a single person or a band.

We also define a few objects:

```ts
const elvis: Person = {
  firstName: "Elvis",
  age: 100,
  hobbies: [
    {
      name: "singing",
    },
  ],
};

const metallica: Band = {
  name: "Metallica",
  members: [
    {
      firstName: "James",
      hobbies: [],
      age: 56,
    },
    {
      firstName: "Lars",
      hobbies: [],
      age: 55,
    },
    {
      firstName: "Kirk",
      hobbies: [],
      age: 57,
    },
    {
      firstName: "Robert",
      hobbies: [],
      age: 55,
    },
  ],
};

const artists: Artist[] = [elvis, metallica];
```

Elvis is a single person and Metallica is a band, together they form the `artists` array.

## Lenses

We'll start with [Lens](https://gcanti.github.io/monocle-ts/modules/index.ts.html#lens-class), which is a composable getter and setter. The signature in `monocle-ts` is

```ts
export class Lens<S, A> {
  constructor(readonly get: (s: S) => A, readonly set: (a: A) => (s: S) => S) { ... }
  ...
}
```

We see that the constructor takes a getter and a setter as input arguments. Type variables `S` and `A` stand for the "source" and "target" types, respectively. The getter consumes a source of type `S` and produces an instance of `A`. The setter is a curried function taking a new value `a` of type `A` and a source object `S` and it returns a new object of type `S`.

Lenses can be created by defining which objects they operate on and the target field. Here's a full example of a lens `personToName` of type `Lens<Person, string>`:

```ts
const personToName: Lens<Person, string> = Lens.fromProp<Person>()("firstName");
```

Type signature `Lens<Person, string>` means that the lens operates on objects of type `Person` and targets a field of type `string`. Lens is created with the static [Lens.fromProp](https://gcanti.github.io/monocle-ts/modules/index.ts.html#fromprop-static-method) method. It requires explicitly setting the type variable `Person`, but it can infer the field type `string` from the type of the field `firstName`. Other ways to create lenses from scratch are the static [fromPath](https://gcanti.github.io/monocle-ts/modules/index.ts.html#frompath-static-method), [fromProps](https://gcanti.github.io/monocle-ts/modules/index.ts.html#fromprops-static-method) and [fromNullableProp](https://gcanti.github.io/monocle-ts/modules/index.ts.html#fromnullableprop-static-method) methods of the `Lens` class. You can also use [LensFromPath](https://gcanti.github.io/monocle-ts/modules/index.ts.html#lensfrompath-interface).

The lens getter `(p: Person) => string` can be accessed via `get` property:

```ts
const getName: (p: Person) => string = (p: Person) => personToName.get(p);
expect(getName(elvis)).toEqual("Elvis");
```

Here's how you could use the `personToName.set` as a setter:

```ts
const setName: (newName: string) => (p: Person) => Person = personToName.set;
const setJillAsName: (p: Person) => Person = setName("Jill");
const modified: Person = setJillAsName(elvis);
expect(modified).toHaveProperty("firstName", "Jill");
expect(elvis).toHaveProperty("firstName", "Elvis"); // Unchanged
```

Note that `elvis` object remains intact as the setter always creates a new copy of the argument.

With the `modify` method you can create a setter that modifies fields with the given function:

```ts
const upperCase = (s: string): string => s.toUpperCase();
const upperCasePersonName: (p: Person) => Person = personToName.modify(
  upperCase
);
const elvisUpperCased = upperCasePersonName(elvis);
expect(elvisUpperCased).toHaveProperty("firstName", "ELVIS");
```

This all nice and good, but the true power of optics becomes clearer when you start to compose them. We'll see examples of this soon when introducing new optics.

## Optional

[`Optional`](https://gcanti.github.io/monocle-ts/modules/index.ts.html#optional-class) is an optic for values that may not exist. The signature is as follows:

```ts
export class Optional<S, A> {
  constructor(readonly getOption: (s: S) => Option<A>, readonly set: (a: A) => (s: S) => S) { ... }
  ...
}
```

Similarly to `Lens`, `Optional` is a generic class with two type variables `S` and `A`. Also similarly to `Lens`, the constructor of `Optional` has input arguments get and set methods, with the exception that `getOption` returns an `Option<A>`. `Option` is a container that may contain a value of type `A`. For an introduction to `Option`, see `fp-ts` [documentation](https://gcanti.github.io/fp-ts/modules/Option.ts.html). Be careful not to confuse the type class `Option` with the optic `Optional`!

Like `Lens`, also `Optional` has many alternatives for constructing one: [`fromPath`](https://gcanti.github.io/monocle-ts/modules/index.ts.html#optionalfrompath-interface), [`fromNullableProp`](https://gcanti.github.io/monocle-ts/modules/index.ts.html#fromnullableprop-static-method-1), [`fromOptionProp`](https://gcanti.github.io/monocle-ts/modules/index.ts.html#fromoptionprop-static-method), and [`OptionalFromPath`](https://gcanti.github.io/monocle-ts/modules/index.ts.html#optionalfrompath-interface). There are good examples in the documentation for how to use them.

For practice, let's construct an `Optional` from scratch. We create an `Optional` that allows accessing the first member of the band. Assuming we allow bands to have no members at all, the first band member may not exist, so we want to safely handle that situation.

Remember we defined our band type as follows:

```ts
type Band = {
  name: string;
  members: Person[];
};
```

Assume that we already have our `members` field of type `Band`, and now we want to access the first member. A function returning the first value of an array is typically called `head`. The type signature for our `head` should then be `Optional<Array<Person>, Person>`. The constructor first takes a `getOption` method of type `(persons: Person[]) => Option<Person>`. Here's how we'd safely get the first member of the band:

```ts
import { some, none } from "fp-ts/lib/Option";

const getOption: (ps: Person[]) => Option<Person> = (personArray: Person[]) =>
  personArray.length === 0 ? none : some(personArray[0]);
```

The helper functions `none` and `some` allow creating options with empty and non-empty values, respectively.

Now we need to define the `set` function for our `Optional<Array<Person>, Person>`. The required signature is `set: (p: Person) => (ps: Person[]) => Person[]`. What is `set` supposed to do? It's supposed the set a person as the first member of the array if the array is not empty. Here's our implementation:

```ts
const set: (p: Person) => (ps: Person[]) => Person[] = (p: Person) => (
  ps: Person[]
) => (ps.length === 0 ? [] : [p, ...ps.slice(1)]);
```

It's very important to notice here what `set` does _not_ do. First, it does not add the given person to the array if the array is empty. `Optional` should only work as a setter when the target value would be non-empty. If the target value is empty, the setter should be no-op. Second, `set` does not prepend given person to the array but replaces the old value with the new value, therefore keeping the length of the list intact.

How's one supposed to know what `set` is supposed to do? The answer lies in optics laws. To be properly composable, every implementation for an optic such as `Lens` or `Optional` must obey specific laws. For Optional, the laws for `getOption` and `set` [are](https://gcanti.github.io/monocle-ts/modules/index.ts.html#optional-class)

1. `getOption(s).fold(() => s, a => set(a)(s)) = s`
1. `getOption(set(a)(s)) = getOption(s).map(_ => a)`
1. `set(a)(set(a)(s)) = set(a)(s)`

The first two laws essentially ensure that `getOption` and `set` are "inverse" operations. The last one states that `set` is idempotent. If `set` added the new value to an empty array, the first law would be violated. If `set` prepended the new value to the existing array, the third law would be violated. I won't go deeper into laws of optics in this article, but beware: when rolling out your own optics, make sure that the laws hold. You may want to use a property based testing library such as [`fastcheck`](https://github.com/dubzzz/fast-check) to be sure.

Now that we've defined `getOption` and `set` for our `Optional` targeting the first value of the band, let's compose it with `members` Lens:

```ts
const membersLens = Lens.fromProp<Band>()("members");
const head: Optional<Array<Person>, Person> = new Optional<
  Array<Person>,
  Person
>(getOption, set);

const bandToFirstMember: Optional<Band, Person> = membersLens.composeOptional(
  head
);
```

We've written our first optics composition! Let's see how it works for a band containing members:

```ts
expect(bandToFirstMember.getOption(metallica)).toEqual(
  some(
    expect.objectContaining({
      firstName: "James",
    })
  )
);
```

The `getOption` returns the first member of the band wrapped in `Option` as expected. Let's try it on an empty band:

```ts
const bandWithNoMembers: Band = {
  name: "Unknown",
  members: [],
};
expect(bandToFirstMember.getOption(bandWithNoMembers)).toEqual(none);
```

In this case `getOption` returns a `none` as expected. Let's go even further and compose `bandToFirstMember` with a lens zooming into the `firstName` property and use it to modify the name:

```ts
const nameLens = Lens.fromProp<Person>()("firstName");
const nameOptional: Optional<Band, string> = bandToFirstMember.composeLens(
  nameLens
);

const upperCase = (s: string): string => s.toUpperCase();

const upperCaseFirstBandMemberName = nameOptional.modify(upperCase);

expect(upperCaseFirstBandMemberName(metallica).members).toContainEqual(
  expect.objectContaining({
    firstName: "JAMES",
  })
);
```

See [artists.test.ts](https://github.com/ksaaskil/functional-programming-examples/blob/master/monocle-ts/artists.test.ts) in the accompanying repository for an example of how to zoom into the oldest member of the band.

`Optional` allows us to zoom into values that may not exist. In the next article, we'll see how `Traversal` and `Fold` can be used to zoom into multiple values (like all members of the band).

## Conclusion

That concludes our introduction to optics with `monocle-ts`! Please leave a comment if you made it all the way to the end, I appreciate all feedback.

Finally, I'd like to mention that I think Giulio Canti's functional programming libraries for TypeScript (`fp-ts`, `monocle-ts`, `io-ts`, `hyper-ts`) all make very good repositories for contributions. Documentation is quite terse and, as far as I know, the author is very open to making the packages more easily approachable to newcomers. So if you read the documentation and find that a killer function is missing documentation or the existing example is broken, shoot a pull request with your own example! I did it too, once :)

## Resources:

- [Introduction to optics](https://medium.com/@gcanti/introduction-to-optics-lenses-and-prisms-3230e73bfcfe) by Giulio Canti
- [A Little Lens Starter Tutorial](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial): Introduction to `lens` package in Haskell
- [Optics reference](https://julien-truffaut.github.io/Monocle/optics.html) from the Monocle documentation
- [Optics in TypeScript](https://medium.com/pleasework/optics-in-typescript-c1a190fb3963)
  by Mike Solomon
- [Control.Lens.Tutorial](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html): Lens tutorial for Haskell beginners
- [python-lenses](https://github.com/ingolemo/python-lenses): Lens library for Python
- [Introduction to Lenses](https://medium.com/javascript-scene/lenses-b85976cb0534) by Eric Elliott
