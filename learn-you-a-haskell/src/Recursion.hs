-- Chapter 5: http://learnyouahaskell.com/recursion
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module Recursion
    ()
where

-- Recursion is important to Haskell because unlike imperative languages, you do computations in Haskell by declaring what something is instead of declaring how you get it. That's why there are no while loops or for loops in Haskell and instead we many times have to use recursion to declare what something is.

-- To set up the maximum function recursively, we first set up an edge condition and say that the maximum of a singleton list is equal to the only element in it. Then we can say that the maximum of a longer list is the head if the head is bigger than the maximum of the tail. If the maximum of the tail is bigger, well, then it's the maximum of the tail.

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
-- maximum' (x:xs) = max x (maximum' xs)  -- Shorter version with max

-- Let's see some more examples to see pattern matching and guards in action!

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _         -- Define edge condition for n with guards as needs boolean condition
    | n<= 0 = []  -- No "otherwise" to allow flowing to next pattern
take' _ []  = []  -- Pattern matching for the list matching part
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Recursion without an edge condition
repeat' :: a -> [a]
repeat' x = x : repeat' x
-- Use with something like `take 4 (repeat' 3)`

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = a == x || a `elem'` xs

-- QuickSort is the poster child of Haskell, it's so elegant!
-- In quicksort, a sorted list is a list that has all the values smaller than (or equal to) the head of the list in front (and those values are sorted), then comes the head of the list in the middle and then come all the values that are bigger than the head (they're also sorted). 
-- Notice that the definition uses the verb "is" to define the algorithm instead of saying "do this, do that, then do that..." That's functional programming!
quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
