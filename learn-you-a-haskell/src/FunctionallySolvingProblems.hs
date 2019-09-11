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
solveRPN expression =
    let splitted = words expression
        folded   = foldl foldingFunction [] splitted
    in  head folded
  where
    foldingFunction (x : y : ys) "*"          = (x * y) : ys
    foldingFunction (x : y : ys) "+"          = (x + y) : ys
    foldingFunction (x : y : ys) "-"          = (x + y) : ys
    foldingFunction xs           numberString = read numberString : xs

solved = solveRPN "10 4 +"
