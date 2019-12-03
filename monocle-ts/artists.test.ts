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
import { findIndex, isEqual, maxBy } from "lodash";

const upperCase = (s: string): string => s.toUpperCase();

interface Hobby {
  name: string;
}

interface Person {
  firstName: string;
  age: number;
  hobbies: Hobby[];
}

type Band = {
  name: string;
  members: Person[];
};

type Artist = Person | Band;

interface Name {
  firstName: string;
  secondName: string;
  lastName: string;
}

const elvis: Artist = {
  firstName: "Elvis",
  age: 100,
  hobbies: [
    {
      name: "singing",
    },
  ],
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

const personToName = Lens.fromProp<Person>()("firstName");

describe("monocle-ts", () => {
  describe("lens", () => {
    it("allows to modify values inside nested object", () => {
      const upperCasePersonName = personToName.modify(upperCase);

      const elvisUpperCased = upperCasePersonName(elvis);

      expect(elvisUpperCased).toMatchObject({ firstName: "ELVIS" });
    });
  });

  describe("optional", () => {
    it("allows working with lists using optionals", () => {
      const oldestOptional = new Optional<Array<Person>, Person>(
        personArray => {
          const oldest = maxBy(personArray, "age");
          return oldest ? some(oldest) : none;
        }, // getOption
        newPerson => personArray => {
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

      const upperCaseOldestBandMember = oldestMemberInBand
        .composeLens(nameLens)
        .modify(upperCase);
      expect(upperCaseOldestBandMember(metallica).members).toContainEqual(
        expect.objectContaining({
          firstName: "KIRK",
        })
      );
      expect(upperCaseOldestBandMember(bandWithNoMembers).members).toEqual([]);
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
      const exampleArtists: Artist[] = [elvis, metallica];

      const artists: Traversal<Artist[], Artist> = fromTraversable(array)<
        Artist
      >();

      const isBand = (a: Artist): a is Band => {
        return Array.isArray((a as any).members);
      };

      const bands: Traversal<Artist[], Band> = artists.composePrism(
        Prism.fromPredicate(isBand)
      );

      const bandNames: Traversal<Artist[], string> = bands.composeLens(
        Lens.fromProp<Band>()("name")
      );

      const upperCaseBandNames: (
        artists: Artist[]
      ) => Artist[] = bandNames.modify((name: string) => name.toUpperCase());

      expect(upperCaseBandNames(exampleArtists)[1]).toHaveProperty(
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
