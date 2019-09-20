module FunctionallySolvingProblems where

import           Data.List                     as L

-- Reverse Polish notation calculator

-- In reverse polish notation, 10 - (4 + 3) * 2 is written as
-- 10 4 3 + 2 * -
-- The idea is that you go from left to right, pushing numbers to the top
-- of the stack. When you encounter an operator, you pop the top two elements
-- from the stack and apply the operator

-- The type for the calculator would be, a function from string to number
solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
 where
  foldingFunction (x : y : ys) "*"          = (x * y) : ys
  foldingFunction (x : y : ys) "+"          = (x + y) : ys
  foldingFunction (x : y : ys) "-"          = (x + y) : ys
  foldingFunction xs           numberString = read numberString : xs

solved = solveRPN "10 4 +"


-- Heathrow to London

-- The job is to make a program that takes input that represents a road system and prints out what the shortest path across it is.

-- Example input looks like this:
{- 50
10
30
5
90
20
40
2
25
10
8
0 -}

-- This can be read in threes, where each section (group of threes) comprises of length of road A, length of road B, and the length of the crossing road.

-- To get the bast path from Heathrow to London, we do this: first we see what the best path to the next crossroads on main road A is. The two options are going directly forward or starting at the opposite road, going forward and then crossing over. We remember the cost and the path. We use the same method to see what the best path to the next crossroads on main road B is and remember that. Then, we see if the path to the next crossroads on A is cheaper if we go from the previous A crossroads or if we go from the previous B crossroads and then cross over. We remember the cheaper path and then we do the same for the crossroads opposite of it. We do this for every section until we reach the end. Once we've reached the end, the cheapest of the two paths that we have is our optimal path!

-- Data type for Section has length of road A, length of road B, and the length of the crossing C.
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

-- The input data is now this:
heathrowToLondon :: RoadSystem
heathrowToLondon =
  [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- Other type declarations we'll need are a label for a road and a possible path
data Label = A | B | C deriving (Eq, Show)
type Path = [(Label, Int)]

-- Solution should have the type:
optimalPath :: RoadSystem -> Path
optimalPath roadSystem = [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8)]
