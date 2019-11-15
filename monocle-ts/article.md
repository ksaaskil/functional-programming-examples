# Optics for the win: monocle-ts ❤ OpenAPI

Optics are a composable way to access and transform complex structures. By a complex structure, I mean something like an [OpenAPI specification](https://swagger.io/specification/) for [Stripe](https://raw.githubusercontent.com/stripe/openapi/master/openapi/spec3.yaml). When working with huge objects like that, you don't want to find yourself writing stuff like `schema.paths["/"].post.responses['200']` too often. Instead, you want to add a layer of abstraction on top of the schema that lets you create complex accessors and transformations by composing simple ones.

That's where optics such as lenses come in. In this article, I'll go through practical examples of using [monocle-ts]() to work with a complex data structure. Other lens libraries include [lens](http://hackage.haskell.org/package/lens) for Haskell, [monocle](https://github.com/julien-truffaut/Monocle) for Scala, and [python-lenses](https://github.com/ingolemo/python-lenses).

As I'm not an expert on optics, I won't go into too much detail in this article. If you want to read more about them, I have found the following resources useful:

- [Lenses](https://medium.com/javascript-scene/lenses-b85976cb0534) by Eric Elliott

You can find the full code for the examples below in [this repository](https://github.com/ksaaskil/functional-programming-examples/tree/master/monocle-ts).

## OpenAPI

As an example data structure, we'll use an OpenAPI specification. OpenAPI specifies the operations supported by a RESTful API. For the purposes of this article, it is sufficient to use the following, hugely simplified representation:

```ts
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
```

An example object could look like the following:

```ts
const openapi: OpenAPI = {
  openapi: "3.0.0",
  info: {
    title: "API title",
    version: "1.0",
  },
  servers: [{ url: "https://api.example.com" }],
  paths: {
    "/": {
      summary: "Root endpoint",
    },
  },
};
```

If you copy-paste the JSON object to [Swagger editor](https://editor.swagger.io/), you can see a pretty online documentation for the API.
