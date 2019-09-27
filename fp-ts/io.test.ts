import { IO, io, map } from "fp-ts/lib/IO";
import { array } from "fp-ts/lib/Array";
// import { sequence_ } from "fp-ts/lib/Foldable";
import { log } from "fp-ts/lib/Console";
import { pipe } from "fp-ts/lib/pipeable";

import { sequenceS } from "fp-ts/lib/Apply";

interface Result {
  name: string;
  age: number;
}

// returns { name: 'Aristotle', age: 60 }

it("Works with IO", () => {
  const logGiraffe: IO<void> = log("giraffe");
  const logZebra: IO<void> = log("zebra");

  const logGiraffeThenZebra: IO<void[]> = array.sequence(io)([
    logGiraffe,
    logZebra,
  ]);

  const ioAction = pipe(
    logGiraffeThenZebra,
    map(() => "Finished")
  );

  const returnResult = ioAction();

  expect(returnResult).toBe("Finished");
});

it("works with sequenceS", () => {
  const computations: { [K in keyof Result]: IO<Result[K]> } = {
    name: io.of("Aristotle"),
    age: io.of(60),
  };

  const computation: IO<Result> = sequenceS(io)(computations);

  console.log(computation());
});
