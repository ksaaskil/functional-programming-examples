// Creating custom typeclass
// https://gcanti.github.io/fp-ts/recipes/HKT.html

export type Identity<A> = A;

import { Functor1 } from "fp-ts/lib/Functor";
export const URI = "Identity";

export type URI = typeof URI;

// URItoKind is type-level map, it maps a URI to a concrete data type, and is populated using the module augmentation feature
declare module "fp-ts/lib/HKT" {
  interface URItoKind<A> {
    Identity: Identity<A>;
  }
}

// URIS in the definition of Functor1 is used as a constraint (keyof URItoKind<any>) in the Functor1 interface:
/** 
export declare type URIS = keyof URItoKind<any>;
export interface Functor1<F extends URIS> {
    readonly URI: F;
    readonly map: <A, B>(fa: Kind<F, A>, f: (a: A) => B) => Kind<F, B>;
}
*/

// `Kind` is defined as follows:
// export declare type Kind<URI extends URIS, A> = URI extends URIS ? URItoKind<A>[URI] : any;
// It is able to project an abstract data type to a concrete data type.
// If URI = 'Identity', then `Kind<URI, number>` is `Identity<number>`.

// Here's how a Functor1 instance is then defined for `Identity`:
export const identityF: Functor1<URI> = {
  URI,
  map: (ma, f) => f(ma),
};

describe("Functor instance for identity", () => {
  it("should map with function", () => {
    const a: Identity<number> = 1;
    expect(identityF.map(a, val => val + 1)).toEqual(2);
  });
});

// For higher-order kinds, see `custom-typeclasses-2.test.ts`.
