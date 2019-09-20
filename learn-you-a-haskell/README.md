# Hello Haskell

Code examples from the awesome [Learn You a Haskell](http://learnyouahaskell.com) book.

- [Chapter 2: Starting out](src/StartingOut.hs)
- [Chapter 3: Types and Typeclasses](./src/TypesAndTypeClasses.hs)
- [Chapter 4: Syntax in Functions](./src/SyntaxInFunctions.hs)
- [Chapter 5: Recursion](./src/Recursion.hs)
- [Chapter 6: Higher order functions](./src/HigherOrderFunctions.hs)
- [Chapter 7: Modules](./src/Modules.hs)
- [Chapter 8: Making our own types and typeclasses](./src/MakingOurOwnTypesAndTypeclasses.hs)
- [Chapter 9: Input and Output](./src/InputAndOutput.hs)
- [Chapter 10: Functionally Solving Problems](./src/FunctionallySolvingProblems.hs)
- [Chapter 11: Functors, Applicative Functors, and Monoids](./src/Functors.hs)

## Project setup

Project was created using [stack](https://docs.haskellstack.org/en/stable/README/):

```bash
stack new hello-haskell new-template
```

### Build

```bash
stack build
```

### Executables

#### Main app

Run [app/Main.hs](./app/Main.hs):

```bash
stack run
```

or

```bash
runhaskell app/Main.hs
```

#### TODO app

Run the [Todo app](./todo-app/Main.hs) from [Chapter 9](http://learnyouahaskell.com/input-and-output):

```bash
stack run todo [command] [filename]
```

For example, this writes to file `todo.txt`:

```bash
stack run todo add todo.txt "Do the laundry."
```

Alternatively, use `runhaskell`:

```bash
runhaskell todo-app/Main.hs add todo.txt "Do the laundry."
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

### Run tests

```bash
stack test
```
