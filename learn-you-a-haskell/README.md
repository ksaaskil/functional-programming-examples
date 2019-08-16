# Hello Haskell

Code examples from the awesome [Learn You a Haskell](http://learnyouahaskell.com) book.

- [Chapter 2: Starting out](src/StartingOut.hs)
- [Chapter 3: Types and Typeclasses](./src/TypesAndTypeClasses.hs)
- [Chapter 4: Syntax in Functions](./src/SyntaxInFunctions.hs)
- [Chapter 5: Recursion](./src/Recursion.hs)
- [Chapter 6: Higher order functions](./src/HigherOrderFunctions.hs)
- [Chapter 7: Modules](./src/Modules.hs)
- [Chapter 8: Making our own types and typeclasses](./src/MakingOurOwnTypesAndTypeclasses.hs)

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
