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

-- Ranges are a way of making lists that are arithmetic sequences of elements that can be enumerated.
listOfNumbers = [1 .. 20]
listOfChars = ['a' .. 'z']
listOfNumbersWithStep = [2, 4 .. 20]
listGoingBackwards = [20, 19 .. 1]
listTakenFromInfiniteList = take 24 [13, 26 ..]
listTakenFromCycle = take 7 (cycle [1, 2, 3])
listWithReplicate = replicate 10 5  -- Equivalent to take 10 (repeat 5)

-- List comprehensions
listComprehension = [ 2 * x | x <- [1 .. 10] ]

-- Filtering a list comprehension using a predicate
listComprehensionWithFilter = [ 2 * x | x <- [1 .. 10], x > 5 ]

-- Custom function applying to a list
boomBangs xs = [ if x < 10 then "BOOM" else "BANG!" | x <- xs, odd x ]

-- Compute all products from two lists
listComprehensionWithTwoLists = [ x * y | x <- [1, 2, 3], y <- [4, 5, 6] ]

-- Simple implementation of length using a list comprehension
length' xs = sum [ 1 | _ <- xs ]

-- Strings are lists of chars so just process them with list comprehensions
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]
