-- Chapter 7: http://learnyouahaskell.com/modules
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module Modules
    ()
where

-- Import given functions
-- import           Data.List                      ( nub )
-- Hide a specific function
-- import           Data.List               hiding ( sort )
-- "Named" import, use as "Map.filter"
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.List                     as L
import qualified Data.Function                 as F

-- All the functions, types and typeclasses that we've dealt with so far were part of the Prelude module, which is imported by default. The syntax for importing modules in a Haskell script is "import <module name>".

numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

-- Functions of modules can be put into the global namescape like this:
-- ghci> :m + Data.List  

-- Examples from Data.List:
interspersed = L.intersperse '.' "Monkey"
intercalated = L.intercalate " " ["hey", "there", "guys"]
transposed = L.transpose [[1, 2, 3], [4, 5, 6]]

-- Summing elements of list
transposeExample =
    map sum $ L.transpose [[0, 3, 5, 9], [10, 0, 0, 9], [8, 6, 1, -1]]

-- L.foldl' and L.foldl1' are stricter versions of their respective lazy incarnations. 
-- The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with thunks like foldl does

concatted = concat ["foo", "bar", "car"]

concatMapped = concatMap (replicate 4) [1 .. 3]

allTrue = and . map (> 4) $ [5, 6, 7, 8]
anyTrue = or $ map (== 4) [2, 3, 4, 5]

anyTrue' = any (== 4) [2, 3, 4, 5]
allTrue' = all (> 4) [6, 49]

-- iterate takes a function and a starting value. It applies the function to the starting value, then it applies that function to the result, then it applies the function to that result again, etc. It returns all the results in the form of an infinite list.

iterated = take 10 $ iterate (* 2) 1

-- Example of dropWhile:
stock =
    [ (994.4 , 2008, 9, 1)
    , (995.2 , 2008, 9, 2)
    , (999.2 , 2008, 9, 3)
    , (1001.4, 2008, 9, 4)
    , (998.3 , 2008, 9, 5)
    ]

firstAboveThousand = head $ dropWhile (\(val, y, m, d) -> val < 1000) stock

-- inits and tails are like init and tail, only they recursively apply that to a list until there's nothing left. 
-- tails "woof" => ["woof", "oof", "of", "f", ""]
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> (take nlen x == needle) || acc)
              False
              (L.tails haystack)

-- partition takes a list and a predicate and returns a pair of lists.
-- Note that span and break are done once they encounter the first element that doesn't and does satisfy the predicate, partition goes through the whole list and splits it up according to the predicate.
partitioned = L.partition (`elem` ['A' .. 'Z']) "BOBsidneyMORGANeddy"

-- find is a safe find function that returns a Maybe
firstOverThousand = L.find (\(val, y, m, d) -> val > 1000) stock

-- Zipping more than two arguments can be done with zipWithX
zippedWith3 = zipWith3 (\x y z -> x + y + z) [1, 2, 3] [4, 5, 2, 2] [2, 2, 3]

-- \\ is the list difference function.
listDiff = [1 .. 10] L.\\ [2, 5, 9]

-- Example of groupBy with `on`:
values =
    [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
groupedBySign = L.groupBy ((==) `F.on` (> 0)) values
-- Here `(==) `F.on` (>0)` is the function \x y -> (==) (>0 x) (>0 y),
-- so it says whether the two values are equal in the sense that they are both larger than zero.

-- Another example with sortBy
xs = [[5, 4, 5, 4, 4], [1, 2, 3], [3, 5, 4, 3], [], [2], [2, 2]]
listsSortedByLength = L.sortBy (compare `F.on` length) xs
-- compare `F.on` length is the same as \x y -> length x `compare` length y

-- Examples of Data.Char
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html

-- Data.Map
-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map.html

-- Data.Set
-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Set.html
