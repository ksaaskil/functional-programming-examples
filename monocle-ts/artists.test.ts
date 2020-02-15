import {
  Iso,
  Lens,
  Optional,
  Prism,
  Traversal,
  fromTraversable,
} from "monocle-ts";
import { array } from "fp-ts/lib/array";
import { some, none } from "fp-ts/lib/Option";
import { Either, fold, isLeft, isRight, getOrElse } from "fp-ts/lib/Either";
import { findIndex, isEqual, maxBy } from "lodash";
import * as t from "io-ts";
import { PathReporter } from "io-ts/lib/PathReporter";

/*
interface Hobby {
  name: string;
}
*/
const HobbyT = t.interface({ name: t.string });
type Hobby = t.TypeOf<typeof HobbyT>; // Static type

/* interface Person {
  firstName: string;
  age: number;
  hobbies: Hobby[];
} */
const PersonT = t.interface({
  firstName: t.string,
  age: t.number,
  hobbies: t.array(HobbyT),
});
type Person = t.TypeOf<typeof PersonT>;

/* type Band = {
  name: string;
  members: Person[];
}; */
const BandT = t.interface({ name: t.string, members: t.array(PersonT) });
type Band = t.TypeOf<typeof BandT>;

// type Artist = Person | Band;
const ArtistT = t.union([PersonT, BandT]);
type Artist = t.TypeOf<typeof ArtistT>;

const elvis: Artist = {
  firstName: "Elvis",
  age: 85,
  hobbies: [
    {
      name: "singing",
    },
  ],
};

/**
 * Small helper function that gets the value from Either if it's right,
 * throws otherwise
 * @param either
 */
const getOrThrow = <A>(either: Either<t.Errors, A>): A => {
  return fold(
    () => {
      throw Error(
        `Failed decoding, errors: ${PathReporter.report(either).join(", ")}`
      );
    },
    (val: A) => val
  )(either);
};

const metallica: Artist = {
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

describe("io-ts", () => {
  it("HobbyT.is accepts an valid hobby object as HobbyT", () => {
    const isHobby = HobbyT.is({ name: "Photographing corgis" });
    expect(isHobby).toBe(true);
  });
  it("HobbyT.is does not accept an invalid hobby object as HobbyT", () => {
    const isHobby = HobbyT.is({ name: 66 });
    expect(isHobby).toBe(false);
  });
  it("HobbyT.decode can decode a hobby from valid input", () => {
    const maybeHobby = HobbyT.decode({ name: "Petting corgis" });
    expect(isRight(maybeHobby)).toBe(true);
  });
  it("HobbyT.decode does not decode a hobby from invalid input", () => {
    const maybeHobby = HobbyT.decode({ name: 67 });
    expect(isLeft(maybeHobby)).toBe(true);
  });
  it("ArtistT.decode can decode an artist from elvis", () => {
    const maybeArtist = ArtistT.decode(elvis);
    expect(isRight(maybeArtist)).toBe(true);
  });
  it("BandT.is validates metallica object as proper Band", () => {
    expect(BandT.is(metallica)).toBe(true);
  });
  it("ArtistT.decode does not decode an artist from invalid data", () => {
    const foo = { lastName: "corgi" };
    const notArtist = ArtistT.decode(foo);
    expect(isLeft(notArtist)).toBe(true);
  });
});

describe("monocle-ts", () => {
  describe("lenses", () => {
    const personToName: Lens<Person, string> = Lens.fromProp<Person>()(
      "firstName"
    );
    it("should be getter", () => {
      const getName: (p: Person) => string = (p: Person) => personToName.get(p);
      expect(getName(elvis)).toEqual("Elvis");
    });
    it("should be a setter", () => {
      const setName: (newName: string) => (p: Person) => Person =
        personToName.set;
      const setJillAsName: (p: Person) => Person = setName("Jill");
      const modified: Person = setJillAsName(elvis);
      expect(modified).toHaveProperty("firstName", "Jill");
      expect(elvis).toHaveProperty("firstName", "Elvis"); // Unchanged
    });
    it("should be a setter", () => {
      const upperCase = (s: string): string => s.toUpperCase();
      const upperCasePersonName: (p: Person) => Person = personToName.modify(
        upperCase
      );
      const elvisUpperCased = upperCasePersonName(elvis);
      expect(elvisUpperCased).toHaveProperty("firstName", "ELVIS");
    });
    it("allows to avoid some boilerplate with 'fromPath'", () => {
      const personToAge: Lens<Person, number> = Lens.fromPath<Person>()([
        "age",
      ]);
      expect(personToAge.get(elvis)).toBe(85);
    });
  });

  describe("optional", () => {
    const oldestOptional = new Optional<Array<Person>, Person>(
      personArray => {
        // How to get the value if exists
        const oldest = maxBy(personArray, "age");
        return oldest ? some(oldest) : none;
      },
      newPerson => personArray => {
        // How to set new value
        const oldest = maxBy(personArray, "age");
        if (typeof oldest === "undefined") {
          return [newPerson];
        }

        const maxIndex = findIndex(personArray, p => isEqual(p, oldest));
        return [
          ...personArray.slice(0, maxIndex),
          newPerson,
          ...personArray.slice(maxIndex + 1),
        ];
      }
    );

    const members = Lens.fromProp<Band>()("members");

    const oldestMemberInBand = members.composeOptional(oldestOptional);

    it("allows working with lists using optionals", () => {
      expect(oldestMemberInBand.getOption(metallica)).toEqual(
        some(
          expect.objectContaining({
            firstName: "Kirk",
          })
        )
      );

      const bandWithNoMembers = { name: "Unknown", members: [] };
      expect(oldestMemberInBand.getOption(bandWithNoMembers)).toEqual(none);

      const nameLens = Lens.fromProp<Person>()("firstName");

      const upperCase = (s: string): string => s.toUpperCase();

      const upperCaseOldestBandMember = oldestMemberInBand
        .composeLens(nameLens)
        .modify(upperCase);

      expect(upperCaseOldestBandMember(metallica).members).toContainEqual(
        expect.objectContaining({
          firstName: "KIRK",
        })
      );
    });
    it("is safe with empty objects", () => {
      const bandWithNoMembers = { name: "Unknown", members: [] };
      expect(oldestMemberInBand.getOption(bandWithNoMembers)).toEqual(none);
    });
  });

  describe.skip("traversal", () => {
    it("allows modifying lists", () => {
      // A Traversal is the generalisation of an Optional to several targets.
      // In other words, a Traversal allows to focus from a type S into 0 to n values of type A.
      const listOfNumbers = [1, 2, 3];
      const traversal: Traversal<number[], number> = fromTraversable(array)<
        number
      >();
      expect(traversal.modify(value => value + 1)(listOfNumbers)).toEqual([
        2,
        3,
        4,
      ]);
    });

    it("allows getting all values via asFold", () => {
      const listOfNumbers = [1, 2, 3];
      const traversal: Traversal<number[], number> = fromTraversable(array)<
        number
      >();
      // Get all values with `asFold`:
      const asFold = traversal.asFold();
      expect(asFold.getAll(listOfNumbers)).toEqual(listOfNumbers);
    });

    it("allows composing with lenses", () => {
      const person: Person = {
        firstName: "Eve",
        age: 67,
        hobbies: [{ name: "swimming" }],
      };

      // Zoom in on hobbies array
      const hobby: Lens<Person, Hobby[]> = Lens.fromProp<Person>()("hobbies");

      // Traversal for hobbies, for example, `person => [{ name: "swimming "}]`
      const hobbies: Traversal<Person, Hobby> = hobby.composeTraversal(
        fromTraversable(array)<Hobby>()
      );
      // Traversal for hobby names, for example: `person => ["swimming"]`
      const hobbyNames: Traversal<Person, string> = hobbies.composeLens(
        Lens.fromProp<Hobby>()("name")
      );

      // Function that uppercases all hobby names
      const upperCaseHobbyNames: (
        p: Person
      ) => Person = hobbyNames.modify((s: string) => s.toUpperCase());

      const personWithUppercasedHobbyNames = upperCaseHobbyNames(person);

      expect(personWithUppercasedHobbyNames.hobbies[0].name).toEqual(
        "SWIMMING"
      );
    });
  });
  describe.skip("prism", () => {
    it("allows zooming in on sum types", () => {
      // A Prism is an optic used to select part of a Sum type, such as types of `Band` in `(Person | Band)[]`

      const artistsT: Traversal<Artist[], Artist> = fromTraversable(array)<
        Artist
      >();

      const isBand = (a: Artist): a is Band => {
        return Array.isArray((a as any).members);
      };

      const bands: Traversal<Artist[], Band> = artistsT.composePrism(
        Prism.fromPredicate(isBand)
      );

      const bandNames: Traversal<Artist[], string> = bands.composeLens(
        Lens.fromProp<Band>()("name")
      );

      const upperCaseBandNames: (
        artists: Artist[]
      ) => Artist[] = bandNames.modify((name: string) => name.toUpperCase());

      expect(upperCaseBandNames(artists)[1]).toHaveProperty(
        "name",
        "METALLICA"
      );
    });
  });
  describe.skip("iso", () => {
    it("allows converting between elements without loss", () => {
      const exampleName: Record<string, string> = {
        firstName: "elvis",
        secondName: "king",
        lastName: "presley",
      };

      const objectToArray = <T>(): Iso<Record<string, T>, Array<[string, T]>> =>
        new Iso<Record<string, T>, Array<[string, T]>>(
          s => Object.entries(s),
          a => a.reduce((q, r) => ({ ...q, [r[0]]: r[1] }), {})
        );

      // Iso from records to an array of key-value pairs
      const recordsIso: Iso<
        Record<string, string>,
        [string, string][]
      > = objectToArray<string>();

      // Traversal that traverses all key-value pairs as tuples
      const records: Traversal<
        Record<string, string>,
        [string, string]
      > = recordsIso.composeTraversal(
        fromTraversable(array)<[string, string]>()
      );

      const upperCaseValues = records.modify(([key, value]) => [
        key,
        value.toUpperCase(),
      ]);

      expect(upperCaseValues(exampleName)).toHaveProperty("firstName", "ELVIS");
    });
  });
});
