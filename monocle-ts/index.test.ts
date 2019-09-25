import { Lens, Optional, Traversal, fromTraversable } from "monocle-ts";
import { array } from "fp-ts/lib/array";
import { some, none } from "fp-ts/lib/Option";

interface Street {
  num: number;
  name: string;
}
interface Address {
  city: string;
  street: Street;
}
interface Company {
  name: string;
  address: Address;
}
interface Employee {
  name: string;
  company: Company;
}

/**
 * Lens from Employee to Company
 */
const company: Lens<Employee, Company> = Lens.fromProp<Employee>()("company");

const address: Lens<Company, Address> = Lens.fromProp<Company>()("address");
const street: Lens<Address, Street> = Lens.fromProp<Address>()("street");
const name: Lens<Street, string> = Lens.fromProp<Street>()("name");

/**
 * Compose lenses
 */
const employeeToStreetName: Lens<Employee, string> = company
  .compose(address)
  .composeLens(street) // Alias for `compose`
  .composeLens(name);

/**
 * Example values
 */

const employee: Employee = {
  name: "john",
  company: {
    name: "awesome inc",
    address: {
      city: "london",
      street: {
        num: 23,
        name: "high street",
      },
    },
  },
};

const capitalize = (s: string): string =>
  s.substring(0, 1).toUpperCase() + s.substring(1);

describe("monocle-ts", () => {
  describe("lenses", () => {
    it("allow to modify values inside nested object", () => {
      /**
       * Modify value with lens (think of `over` in Control.Lens)
       */
      const employee2 = employeeToStreetName.modify(capitalize)(employee);

      expect(employee2.company.address.street.name).toMatch(/^High/);
    });
    it("allows to avoid some boilerplate with 'fromPath'", () => {
      const employeeToStreetName = Lens.fromPath<Employee>()([
        "company",
        "address",
        "street",
        "name",
      ]);
      const employee2 = employeeToStreetName.modify(capitalize)(employee);
      expect(employeeToStreetName.get(employee2)).toMatch(/^High/);
    });
  });

  describe("optionals", () => {
    it("allow composing with optionals for nullable values", () => {
      // Optional that allows zooming into the (optional) first letter
      const firstLetter = new Optional<string, string>(
        s => (s.length > 0 ? some(s[0]) : none), // getOption
        a => s => a + s.substring(1) // set
      );
      const toFirstLetter: Optional<Employee, string> = company
        .compose(address)
        .compose(street)
        .compose(name)
        .asOptional()
        .compose(firstLetter);
      const upperCaseStreetName: (
        e: Employee
      ) => Employee = toFirstLetter.modify(s => s.toUpperCase());
      const employeeToStreetName = Lens.fromPath<Employee>()([
        "company",
        "address",
        "street",
        "name",
      ]);
      expect(employeeToStreetName.get(upperCaseStreetName(employee))).toMatch(
        /^High/
      );
    });
    it("allow working with lists using optionals", () => {
      const firstNumber = new Optional<Array<number>, number>(
        s => (s.length > 0 ? some(s[0]) : none), // getOption
        a => s => [a, ...s.slice(1)] // Set value by replacing the first value in the array
      );

      expect(firstNumber.getOption([1, 2, 3])).toEqual(some(1));
      expect(firstNumber.getOption([])).toEqual(none);

      const addOneToFirstNumber = firstNumber.modify(value => value + 1);
      expect(addOneToFirstNumber([1, 2, 3])).toEqual([2, 2, 3]);
      expect(addOneToFirstNumber([])).toEqual([]);
    });
  });

  describe("traversals", () => {
    it("allow modifying lists", () => {
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
    it("allow getting all values via asFold", () => {
      const listOfNumbers = [1, 2, 3];
      const traversal: Traversal<number[], number> = fromTraversable(array)<
        number
      >();
      // Get all values with `asFold`:
      const asFold = traversal.asFold();
      expect(asFold.getAll(listOfNumbers)).toEqual(listOfNumbers);
    });
  });
});
