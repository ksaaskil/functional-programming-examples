// https://dev.to/gcanti/getting-started-with-fp-ts-semigroup-2mf7
import { semigroupSum, Semigroup } from "fp-ts/lib/Semigroup";
import { getApplySemigroup, Option, some, none } from "fp-ts/lib/Option";

// # Semigroups

// A semigroup is a pair (A, *) in which A is a non-empty set and * is a binary
// associative operation on A:
// *: (x: A, y: A) => A
// with associativity law
// (x * y) * z = x * (y * z)

// Type class for Semigroup is defined as follows:
interface Semigroup1<A> {
  concat: (x: A, y: A) => A;
}

// `concat` operation can mean concatenation, merging, fusion, selection, addition, substitution, etc.

// Example instance for type `number` with `product` operation:
const semigroupProduct: Semigroup1<number> = {
  concat: (x, y) => x * y,
};

// The same with addition:
const semigroupAddition: Semigroup1<number> = {
  concat: (x, y) => x + y,
};

// And strings
const semigroupString: Semigroup1<string> = {
  concat: (x, y) => x + y,
};

// These trivial semigroups always exist for any type:
const getFirstSemigroup = <A = never>(): Semigroup1<A> => {
  return { concat: (x, y) => x };
};

const getLastSemigroup = <A = never>(): Semigroup1<A> => {
  return { concat: (x, y) => y };
};

// The free semigroup for A is a semigroup instance for (NonEmpty) Array<A> under array concatenation:
const getArraySemigroup = <A = never>(): Semigroup1<Array<A>> => {
  return { concat: (x, y) => x.concat(y) };
};

// Semigroups for type constructors
// To merge some(a) with some(b), we need something to merge a and b. That's what semigroups do!

const S: Semigroup<Option<number>> = getApplySemigroup(semigroupSum);

it("concatenates options", () => {
  expect(S.concat(some(1), some(2))).toEqual(some(3));
  expect(S.concat(some(1), none)).toEqual(none);
});
