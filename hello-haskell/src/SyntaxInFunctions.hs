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

-- One can define values to avoid repeating oneself using the "where" binding.
-- Where bindings are a syntactic construct that let you bind to variables
-- at the end of a function and the whole function can see them, including all the guards.
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
  where
    bmi                   = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)  -- Pattern matching


-- One can also define functions inside "where"
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]  -- Pattern matching in a list comprehension!
    where bmi weight height = weight / height ^ 2  -- Function defined in a binding!

-- Let bindings let one bind to variables anywhere and are expressions themselves.
-- They are very local, so they don't span across guards.
-- The form is "let <bindings> in <expression>":
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in  sideArea + 2 * topArea

-- let bindings are expressions that one can cram basically anywhere
-- just like if-else:
expressionWithLet = 4 * (let a = 9 in a + 1) + 2

-- One can also introduce functions in local scope:
squares = [let square x = x * x in (square 5, square 3, square 2)]

-- Several variables are separated with semicolons:
tupleWithLets =
    ( let a = 100
          b = 200
          c = 300
      in  a * b * c
    , let foo = "Hey "
          bar = "there!"
      in  foo ++ bar
    )

-- Remember you can pattern match bindings:
foo = (let (a, b, c) = (1, 2, 3) in a + b + c) * 100

-- let bindings can also be used inside list comprehensions:
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2 ]

-- Case expressions can be used for pattern matching pretty much anywhere:
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
    []  -> "empty."
    [x] -> "a singleton list."
    xs  -> "a longer list."


-- This is equivalent to this
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where
    what []  = "empty."
    what [x] = "a singleton list."
    what xs  = "a longer list."
