-- http://learnyouahaskell.com/starting-out
-- Load with `:l StartingOut` from `stack ghci`
module StartingOut
    ()
where

-- Function with no arguments
-- Apply with: printHello (no parentheses!)
printHello = putStrLn "Hello from Haskell!"

-- Function taking one argument
-- Apply with: doubleMe 2
doubleMe x = x + x

-- Function taking two arguments
-- Application with prefix: doubleUs 1 2
-- Application as infix: 1 `doubleUs` 2
doubleUs x y = doubleMe x + doubleMe y

-- In Haskell, if statement is an expression and must return something
doubleSmallNumber x = if x > 100 then x else x * 2

-- Use the expression as part of a larger expression
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
