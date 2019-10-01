// For higher-order type constructors, there are `URItoKind2`, `URIS2`, and `Kind2`:

// export declare type URIS2 = keyof URItoKind2<any, any>;
// export declare type Kind2<URI extends URIS2, E, A> = URI extends URIS2 ? URItoKind2<E, A>[URI] : any;

import { Functor2 } from "fp-ts/lib/Functor";

export const URI = "Either";
export type URI = typeof URI;

declare module "fp-ts/lib/HKT" {
  interface URItoKind2<E, A> {
    Either: Either<E, A>;
  }
}

export interface Left<E> {
  readonly _tag: "Left";
  readonly left: E;
}

export interface Right<A> {
  readonly _tag: "Right";
  readonly right: A;
}

export type Either<E, A> = Left<E> | Right<A>;

const right = <A>(a: A): Right<A> => ({
  _tag: "Right",
  right: a,
});

const left = <E>(e: E): Left<E> => ({
  _tag: "Left",
  left: e,
});

export const eitherF: Functor2<URI> = {
  URI,
  map: (ma, f) => (ma._tag === "Left" ? ma : right(f(ma.right))),
};

describe("Functor2 for Either", () => {
  it("should work for right", () => {
    const val: Either<Error, number> = right(1);
    const val2 = eitherF.map(val, (val: number) => val + 1);
    expect(val2).toEqual(right(2));
  });
  it("should work for left", () => {
    const val: Either<Error, number> = left(Error("text"));
    const val2 = eitherF.map(val, (val: number) => val + 1);
    expect(val2).toEqual(val);
  });
});
