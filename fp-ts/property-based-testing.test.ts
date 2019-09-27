// https://dev.to/gcanti/introduction-to-property-based-testing-17nk
import * as fc from "fast-check";
import { Semigroup } from "fp-ts/lib/Semigroup";

// Semigroup that's not a monoid because of missing "empty"
const S: Semigroup<string> = {
  concat: (x, y) => x + " " + y,
};

// Expected law
const associativity = (x: string, y: string, z: string) =>
  S.concat(S.concat(x, y), z) === S.concat(x, S.concat(y, z));

// An Arbitrary<A> generates random values of type A:
const stringArb: fc.Arbitrary<string> = fc.string();

describe("Semigroup instance", () => {
  it("should satisfy associativity", () => {
    fc.assert(fc.property(stringArb, stringArb, stringArb, associativity));
  });
});
