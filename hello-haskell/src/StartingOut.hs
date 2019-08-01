-- http://learnyouahaskell.com/starting-out
-- Load with `:l StartingOut` from `stack ghci`
module StartingOut
    ()
where

-- Function with no arguments is called a definition or name
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

-- In Haskell, lists are homogenous: they can only store elements of the same type.
lostNumbers = [4, 8, 15, 16, 23, 42]

-- Concatenation with ++
concatenatedLists = lostNumbers ++ [9, 10, 11, 12]

-- Strings are just lists of chars
concatenatedString = "hello" ++ " " ++ "world"

-- Cons operator is very fast compared to concatenation
aSmallCat = 'A' : " SMALL CAT"

-- These are equal
areEqualLists = 1 : 2 : 3 : [] == [1, 2, 3]  -- true

-- Accessing elements by index
secondLetter = "Steve Buscemi" !! 1

-- Lists have the usual basic functions
headOfAList = head lostNumbers
lengthOfAList = length lostNumbers
listIsNull = null lostNumbers
firstThreeNumbers = take 3 lostNumbers
has42 = 42 `elem` lostNumbers
