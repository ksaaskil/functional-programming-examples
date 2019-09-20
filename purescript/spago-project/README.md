# Example project using Spago in [2019](https://discourse.purescript.org/t/recommended-tooling-for-purescript-applications-in-2019/948)

### Project initialization

Start project:

```
yarn init -y
```

Initialize PureScript project with Spago:

```
yarn spago init
```

This command creates, among others, `packages.dhall` and `spago.dhall`.

This is how `packages.dhall` looks like for me at the time of writing (excluding comments):

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190831/packages.dhall sha256:852cd4b9e463258baf4e253e8524bcfe019124769472ca50b316fe93217c3a47

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
```

These are the contents of `spago.dhall`:

```dhall
{ name =
    "my-project"
, dependencies =
    [ "effect", "console", "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
```

Build project:

```
yarn spago build
```

The project is built in `output/` directory.

Run `Main.purs`:

```
yarn spago run
```

### Install dependencies

Install Halogen:

```
yarn spago install halogen
```

`spago.dhall` now looks like this:

```dhall
{
 name =
    "my-project"
, dependencies =
    [ "console", "effect", "halogen", "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
```
