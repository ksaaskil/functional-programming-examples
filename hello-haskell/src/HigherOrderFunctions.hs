-- Chapter 6: http://learnyouahaskell.com/higher-order-functions
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module HigherOrderFunctions
    ()
where

-- Haskell functions can take functions as parameters and return functions as return values. A function that does either of those is called a higher order function. 

-- All functions in Haskell are curried, i.e., they take one parameter at a time. For example,
a = max 4 5
-- can be read as
a' = (max 4) 5
-- where (max 4) is a function applied to 5. Similarly, the type of max can be written as
-- max :: (Ord a) => a -> (a -> a).

-- In Haskell, putting a space between things is function application. It is like an operator with the highest precedence.

-- A function called with "too few" parameters (like max 4) is a partially applied function.
-- For example, to create function that compares a number to 10 we can do
compareWithTen :: (Num a, Ord a) => a -> Ordering
compareWithTen = compare 10
-- where "compareWithTen x = compare 10 x" is equivalent!

-- Also infix functions can be partially applied by using sections. To section an infix function, surround it with parentheses and only supply a parameter on one side.
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

-- Similarly, a function that checks if a character belongs to uppercase letters:
isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A' .. 'Z'])

-- Functions can take functions as parameters:
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- Note that parentheses are needed in the type because `->` is right-associative.
-- `applyTwice` is essentially a function of two arguments, a function and a value,
-- and it returns a value.
sixteen = applyTwice (+ 3) 10
sentence = applyTwice (++ " HAHA") "HEY"  -- HEY HAHA HAHA
sentence2 = applyTwice ("HAHA " ++) "HEY" -- HAHA HAHA HEY

-- A bit more useful example, zipWith:
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- "As you can see, a single higher order function can be used in very versatile ways. Imperative programming usually uses stuff like for loops, while loops, setting something to a variable, checking its state, etc. to achieve some behavior and then wrap it around an interface, like a function. Functional programming uses higher order functions to abstract away common patterns, like examining two lists in pairs and doing something with those pairs or getting a set of solutions and eliminating the ones you don't need."

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where g x y = f y x
-- Or even simpler:
-- flip' f x y = g y x
-- Note that one can here take advantage of currying when making higher-order functinos by thinking ahead and writing what their end result would be if they were fully applied and flip' was considered as type "flip' :: (a -> b -> c) -> b -> a -> c"

-- Example:
flipped = flip' zip [1, 2, 3, 4, 5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]
flipped2 = zipWith' (flip' div) [2, 2 ..] [10, 8, 6, 4, 2]
-- [5,4,3,2,1]
