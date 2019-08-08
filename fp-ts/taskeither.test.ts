import {
  TaskEither,
  tryCatch,
  chain,
  map,
  fromEither,
} from "fp-ts/lib/TaskEither";
import { either, tryCatch as eitherTryCatch } from "fp-ts/lib/either";
import { pipe } from "fp-ts/lib/pipeable";
import { Lazy } from "fp-ts/lib/function";
import axios, { AxiosResponse } from "axios";
import chalk from "chalk";

const ICNDB_HOST = "http://api.icndb.com";

const colorJoke = chalk.bold.magentaBright;
const url = `${ICNDB_HOST}/jokes/random`;

const chuckClient = async (): Promise<string> => {
  const response = await axios.get(url);
  const joke = response.data.value.joke;
  return joke;
};

const fetch = async (url: string) => {
  return await axios(url);
};

type ResponseBody = any;

const fpFetch = (url: string): TaskEither<Error, ResponseBody> =>
  tryCatch(
    async () => {
      const axiosResponse = await fetch(url);
      return axiosResponse.data;
    },
    (e: unknown) => new Error(`Failed fetching from ${url}`)
  );

const liftToEither = <A>(f: Lazy<A>, failureMessage?: string) =>
  eitherTryCatch(f, (e: unknown) =>
    Error(failureMessage || `Operation failed: ${JSON.stringify(e)}`)
  );

const parseResponseBody = (responseBody: any): string =>
  responseBody.value.joke;

const fpChuckClient = (url: string): TaskEither<Error, string> =>
  pipe(
    url,
    fpFetch,
    chain(responseBody =>
      fromEither(
        liftToEither(
          () => parseResponseBody(responseBody),
          `Failed parsing joke from response`
        )
      )
    )
  );

describe("Getting a random joke", () => {
  it("works", async () => {
    const joke = await chuckClient();
    console.log(`Your daily Chuck joke: ${colorJoke(joke)}`);
    expect(joke.length).toBeGreaterThan(0);
  });
});
