-- Chapter 4: http://learnyouahaskell.com/syntax-in-functions
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module SyntaxInFunctions
    ()
where

-- Pattern matching

-- When defining functions, define separate bodies for different patterns
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry you're out of luck!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe x = "Not one or two!"

-- Recursion becomes beautiful with pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Extract variables with pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Custom `head`
head' :: [a] -> a
head' []      = error "Can't call head on an empty list!"
head' (x : _) = x
-- head' [x] = x  -- This would also work for matching a list

-- Custom length function:
length' :: (Num b) => [a] -> b
length' []       = 0
length' (_ : xs) = 1 + length' xs

-- "as patterns" allow keeping reference to the full input
capital :: String -> String
capital ""           = "Empty string!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards are indicated by pipes that follow a function's name and its parameters.
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi | bmi < 18.5 = "You're underweight, you emo, you!"
            | bmi < 25.0 = "You're supposedly normal, pfft!"
            | bmi < 30.0 = "You're fat you fatty!"
            | otherwise  = "You're a whale, congrats!"

max' :: (Ord a) => a -> a -> a
max' a b | a < b     = a
         | otherwise = b

-- Custom compare with infix definition using backticks
compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b | a > b  = GT
               | a < b  = LT
               | a == b = EQ
