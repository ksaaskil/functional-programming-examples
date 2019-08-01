# Hello Haskell

Code examples from the awesome [Learn You a Haskell](http://learnyouahaskell.com) book. See the `src` code for chapter notes.

## Project setup

Project was created using [stack](https://docs.haskellstack.org/en/stable/README/):

```bash
stack new hello-haskell new-template
```

### Build

```bash
stack build
```

### Interactive mode

```bash
stack ghci
```

Load a module in `ghci`:

```
Prelude> :l StartingOut
```

Reload module:

```
StartingOut> :r
```

### Execute

```bash
stack exec hello-haskell-exe
```

### Test

```bash
stack test
```
