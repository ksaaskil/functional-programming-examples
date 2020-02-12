# Introduction to monocle-ts and io-ts

Giulio Canti's [monocle-ts](https://github.com/gcanti/monocle-ts) library is a partial TypeScript port of Scala's [Monocle](https://julien-truffaut.github.io/Monocle/) library. The latter is described as "an optics library for Scala (and Scala.js) strongly inspired by Haskell Lens." Haskell's [lens](https://hackage.haskell.org/package/lens) library provides "a highly generic toolbox for composing families of getters, folds, isomorphisms, traversals, setters and lenses and their indexed variants."

It is quite easy to get overwhelmed by all of this (I am), so I thought I'd write a small tutorial for practical applications of `monocle-ts`. No Haskell knowledge is required.

We'll use the [io-ts](https://github.com/gcanti/io-ts) library for defining the types in our `monocle-ts` examples. Using `io-ts` adds a small overhead to the article but I think it's useful to understand how it works together with `monocle-ts`. If you know `io-ts` already, you can skip the first section.

The code for this tutorial can be found from [monocle-ts folder](https://github.com/ksaaskil/functional-programming-examples/tree/master/monocle-ts) of [this repository](https://github.com/ksaaskil/functional-programming-examples).

## Getting started with `io-ts`

`io-ts` is a run-time type system. Basically it allows you to safely type-check those pesky `Any` objects you get from external sources like user inputs, files, or databases. Let's consider a simple `Hobby` interface defined as follows:

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

I use the `T` extension to mark `io-ts` types. It's important to notice that the `HobbyT` is a `const`: it's an object that remembers its properties even after the code is transpiled to JavaScript. Therefore, one can use the `HobbyT` object at _run-time_ to check if objects are actually valid hobbies or not.

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

`decode` method returns an [Either](https://gcanti.github.io/fp-ts/modules/Either.ts.html) object, where the value can be "left" or "right" corresponding to failure or success, respectively. If there's an error, the either contains a "left" of `t.Errors` type defined as follows:

```ts
export interface Errors extends Array<ValidationError> {}
```

Validation errors can be printed with, for example, the [PathReporter](https://github.com/gcanti/io-ts#error-reporters) utility.

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

## Traversals

## Conclusion and resources

Resources:

- [A Little Lens Starter Tutorial](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial): Introduction to `lens` package in Haskell
- [Control.Lens.Tutorial](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html): Lens tutorial for Haskell beginners
- [python-lenses](https://github.com/ingolemo/python-lenses): Lens library for Python

## Throwaway

That's where optics such as lenses come in. In this article, I'll go through practical examples of using [monocle-ts]() to work with a complex data structure. Other lens libraries include [lens](http://hackage.haskell.org/package/lens) for Haskell, [monocle](https://github.com/julien-truffaut/Monocle) for Scala, and [python-lenses](https://github.com/ingolemo/python-lenses).

As I'm not an expert on optics, I won't try to go into detail in this article. If you want to learn more about optics, I have found the following resources useful:

- [Lenses](https://medium.com/javascript-scene/lenses-b85976cb0534) by Eric Elliott

You can find the full code for the examples below in [this repository](https://github.com/ksaaskil/functional-programming-examples/tree/master/monocle-ts).
