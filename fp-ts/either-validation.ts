// Handling validation with multiple errors, copied from
// https://dev.to/gcanti/getting-started-with-fp-ts-either-vs-validation-5eja
// Comments added by me.
import { sequenceT } from "fp-ts/lib/Apply";
import {
  Either,
  getValidation,
  left,
  map,
  mapLeft,
  right,
} from "fp-ts/lib/Either";
import { getSemigroup, NonEmptyArray } from "fp-ts/lib/NonEmptyArray";
import { pipe } from "fp-ts/lib/pipeable";
import expect from "expect";

// A "check" function checking length
const minLength = (s: string): Either<string, string> =>
  s.length >= 6 ? right(s) : left("at least 6 characters");

const oneCapital = (s: string): Either<string, string> =>
  /[A-Z]/g.test(s) ? right(s) : left("at least one capital letter");

const oneNumber = (s: string): Either<string, string> =>
  /[0-9]/g.test(s) ? right(s) : left("at least one number");

/**
 * Lift a check function returning an either to a check function returning an either of an array
 * @param check Check function returning an `Either` from a value
 */
function lift<L, A>(
  check: (a: A) => Either<L, A>
): (a: A) => Either<NonEmptyArray<L>, A> {
  return a =>
    pipe(
      check(a),
      mapLeft(a => [a])
    );
}

/**
 * A semigroup for concatenating arrays of strings with the `.concat` property
 */
const StringSemiGroup = getSemigroup<string>();

expect(StringSemiGroup.concat(["a"], ["b"])).toEqual(["a", "b"]);

const ApplicativeValidation = getValidation(StringSemiGroup);

// Validation has `map` to map the `right` value
const exampleOfMap: Either<
  NonEmptyArray<string>,
  { verified: boolean }
> = ApplicativeValidation.map(lift(minLength)("asdfasdf"), _ => ({
  verified: true,
}));

console.log("Result of mapping:", exampleOfMap);

const exampleOfChaining: Either<
  NonEmptyArray<string>,
  string
> = ApplicativeValidation.chain(lift(minLength)("st"), lift(oneCapital));

console.log("Result of chaining checks:", exampleOfChaining);

const fullCheck = (
  s: string
): Either<NonEmptyArray<string>, [string, string, string]> =>
  sequenceT(ApplicativeValidation)(
    lift(minLength)(s),
    lift(oneCapital)(s),
    lift(oneNumber)(s)
  );

console.log("Result of full check:", fullCheck("st"));
console.log("Result of full check for valid string:", fullCheck("ASDFasdf2"));

function validatePassword(s: string): Either<NonEmptyArray<string>, string> {
  return pipe(
    sequenceT(ApplicativeValidation)(
      lift(minLength)(s),
      lift(oneCapital)(s),
      lift(oneNumber)(s)
    ),
    map(() => s)
  );
}

console.log(validatePassword("ab"));
