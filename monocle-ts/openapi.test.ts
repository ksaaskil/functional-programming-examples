/**
 * Examples of using monocle-ts with OpenAPI objects.
 * Only a small portion of full OpenAPI schema is included below
 * for illustration.
 */
import * as jsYaml from "js-yaml";
import * as fs from "fs";

export interface OpenAPI {
  openapi: string;
  info: Info;
  paths: Paths;
  servers?: Array<Server>;
}

export interface Info {
  title: string;
  description?: string;
  version: string;
}

export type Paths = Record<string, PathItem>;

export interface PathItem {
  summary?: string;
  description?: string;
}

export interface Server {
  url: string;
}

const openapi: OpenAPI = jsYaml.safeLoad(
  fs.readFileSync("./stripe-simple.yaml", "utf-8")
);

describe("Lenses", () => {
  it("should have 1=1", () => {
    expect(1).toBe(1);
  });
});
