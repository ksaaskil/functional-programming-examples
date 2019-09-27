import { contramap, Eq, getStructEq } from "fp-ts/lib/Eq";

// https://dev.to/gcanti/getting-started-with-fp-ts-setoid-39f3
// # Type classes in fp-ts
// In fp-ts, type classes are encoded as TypeScript interfaces

// Type class `Eq` is defined as follows:
interface Eq1<A> {
  // Name as Eq1 to avoid collisions with `fp-ts/lib/Eq`
  readonly equals: (x: A, y: A) => boolean;
}

// Read as: "a type A belongs to type class Eq if there is a function named `equal` of the appropriate type defined on it".

// Instances of a type class Eq for type A can be made by declaring an instance that defines implementations
// of all of Eq's members for A.

// Example:
const eqNumber: Eq1<number> = {
  equals: (x, y) => x === y,
};

/**
 * For typeclass Eq, we expect instances to satisfy
 * 1. Reflexivity
 * 2. Symmetry
 * 3. Transitivity
 */

// A function of `elem` could then be defined for members of typeclass Eq as follows:
const elem = <A>(E: Eq1<A>): ((a: A, as: Array<A>) => boolean) => {
  return (a, as) => as.some(item => E.equals(item, a));
};

// For structs, one can derive an instance of `Eq` using `getStructEq` from `fp-ts/lib/Eq`:
type Point = {
  x: number;
  y: number;
};

const eqPoint: Eq<Point> = getStructEq({
  x: eqNumber,
  y: eqNumber,
});

// Going further:

type Vector = {
  from: Point;
  to: Point;
};

const eqVector: Eq<Vector> = getStructEq({
  from: eqPoint,
  to: eqPoint,
});

// Finally, one can use `contramap` to derive an instance of `Eq` for `B` given
// an instance of `Eq` for `A` and a function from `B` to `A`:

type User = {
  userId: number;
  name: string;
};

// Two users are equal if their `userId` fields are equal
const eqUser: Eq<User> = contramap((user: User) => user.userId)(eqNumber);

it("should recognize two users as equal", () => {
  const user1 = { userId: 23, name: "K" };
  const user2 = { userId: 23, name: "M" };
  expect(eqUser.equals(user1, user2)).toBe(true);
});
