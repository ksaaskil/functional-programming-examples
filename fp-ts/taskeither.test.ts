import {
  TaskEither,
  tryCatch,
  chain,
  map,
  fromEither,
} from "fp-ts/lib/TaskEither";
import {
  either,
  Either,
  fold,
  isLeft,
  isRight,
  getOrElse,
  tryCatch as eitherTryCatch,
} from "fp-ts/lib/either";
import { pipe } from "fp-ts/lib/pipeable";
import { Lazy } from "fp-ts/lib/function";
import axios from "axios";
import chalk from "chalk";

const ICNDB_HOST = "http://api.icndb.com";

const colorJoke = chalk.bold.magentaBright;
const url = `${ICNDB_HOST}/jokes/random`;

/**
 * Fetch response body from URL.
 * @param url Target URL
 */
const fetch = async (url: string) => {
  return (await axios(url)).data;
};

const parseResponseBody = (responseBody: any): string =>
  responseBody.value.joke;

const chuckClient = async (): Promise<string> => {
  const response = await fetch(url);
  const joke = parseResponseBody(response);
  return joke;
};

type ResponseBody = any;

const fetchJokeTask = (url: string): TaskEither<Error, ResponseBody> =>
  tryCatch(
    () => fetch(url),
    (e: unknown) => new Error(`Failed fetching from ${url}`)
  );

const asTaskEither = <A>(
  f: Lazy<A>,
  failureMessage?: string
): TaskEither<Error, A> => fromEither(asEither(f, failureMessage));

const asEither = <A>(f: Lazy<A>, failureMessage?: string) =>
  eitherTryCatch(f, (e: unknown) =>
    Error(failureMessage || `Operation failed: ${JSON.stringify(e)}`)
  );

const parseResponseBodyAsTaskEither = (
  responseBody: any
): TaskEither<Error, string> =>
  asTaskEither(
    () => parseResponseBody(responseBody),
    `Failed parsing joke from response: ${JSON.stringify(responseBody)}`
  );

const chainWithEither: <A, B>(
  f: (fa: A) => Either<Error, B>
) => (te: TaskEither<Error, A>) => TaskEither<Error, B> = <A, B>(
  f: (fa: A) => Either<Error, B>
) => {
  return (te: TaskEither<Error, A>) => chain((a: A) => fromEither(f(a)))(te);
};

// Problem: chaining a `TaskEither<Error, string>` with `string => Either<Error, string>`
// (without casting everything with `asTaskEither`) is possible how?
const fpChuckClient = (url: string): TaskEither<Error, string> =>
  pipe(
    url,
    fetchJokeTask,
    chainWithEither(responseBody =>
      asEither(() => parseResponseBody(responseBody))
    )
  );

/**
 * Get the right value or throw.
 * @param ma Instance of either
 */
const get = <A, B>(ma: Either<A, B>): B => {
  return fold(
    e => {
      throw e;
    },
    (val: B) => val
  )(ma);
};

describe("Getting a random joke", () => {
  it("works imperatively", async () => {
    const joke = await chuckClient();
    console.log(`Your daily Chuck joke: ${colorJoke(joke)}`);
    expect(joke.length).toBeGreaterThan(0);
  });
  it("works with FP", async () => {
    const jokeTask: TaskEither<Error, string> = fpChuckClient(url);
    const resolvedJoke: Either<Error, string> = await jokeTask();
    const joke = get(resolvedJoke);
    console.log(`Your daily Chuck joke: ${colorJoke(joke)}`);
    expect(joke.length).toBeGreaterThan(0);
  });
});
