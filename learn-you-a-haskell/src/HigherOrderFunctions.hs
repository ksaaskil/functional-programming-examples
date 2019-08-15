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

-- Maps and filters
map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

addedThree = map' (+ 3) [1, 4, 3, 2]

replicated = map (replicate 3) [3 .. 6]

allSquared = map (map (^ 2)) [[1, 2], [3, 5]]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []       = []
filter' f (x : xs) = if f x then x : filter f xs else filter f xs
-- Or with guards
-- filter f (x:xs) | f x = x : filter f xs | otherwise = filter f xs

filtered = filter' (> 3) [1, 5, 10, 2]

notNulls =
    let notNull x = not (null x)
    in  filter notNull [[1, 2, 3], [], [3, 4, 5], [2, 2], [], [], []]

-- quicksort with filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
    let smallerThan = filter (<= x) xs
        greaterThan = filter (> x) xs
    in  quicksort smallerThan ++ [x] ++ quicksort greaterThan

largestDivisible :: (Integral a) => a
largestDivisible = maximum
    (let divisible x = x `mod` 3829 == 0 in filter' divisible [0 .. 100000])
-- or, better, use Haskell's laziness:
-- largestDivisible = head (filter p [100000, 99999 ..]) where p x = x `mod` 3829 == 0

-- Find the sum of all odd squares that are smaller than 10000
-- Without takeWhile, one does not know which range to start with (cannot be infinite)
sumOfAllSquares' = sum (filter (< 10000) (filter odd (map (^ 2) [1 .. 10000])))
-- With takeWhile, one can use an infinite range:
sumOfAllSquares = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

-- Collatz sequence: Take a natural number. If that number is even, divide it by two. If it's odd, multiply it by 3 and then add 1 to that. We take the resulting number and apply the same thing to it, which produces a new number and so on.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n    = n : chain (n `div` 2)
        | otherwise = n : chain (3 * n + 1)

-- For all starting numbers between 1 and 100, how many chains have a length greater than 15?
chainsLongerThan15 :: Int
chainsLongerThan15 = length (filter isLong (map chain [1 .. 100]))
    where isLong xs = length xs > 15

-- Note that one can also map with functions taking more than one argument to produce a list of functions:
multipliers = map (*) [0 ..]
ten = (multipliers !! 2) 5  -- second element of the list is (2*)

-- Lambdas are anonymous functions used only once, normally with the sole purpose of passing it to a higher-order function. To make a lambda, write a "\" followed by the parameters, separated by spaces. After that comes a -> and the function body.

-- Rewrite chainsLongerThan15 with a lambda:
chainsLongerThan15' :: Int
chainsLongerThan15' =
    length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

-- Lambdas are expressions so one can pass them around like that. 

-- Lambdas can take any number of arguments:
zipped = zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]

-- One can also pattern match a (single) pattern with lambdas:
mapped = map (\(a, b) -> a + b) [(1, 2), (3, 4)]

-- One can use lambdas in function expressions when it's more readable:
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- A fold takes a binary function, a starting value (accumulator) and a "foldable" like a list.
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- More succinctly: sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
-- map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- "One big difference [between left and right folds] is that right folds work on infinite lists, whereas left ones don't! To put it plainly, if you take an infinite list at some point and you fold it up from the right, you'll eventually reach the beginning of the list. However, if you take an infinite list at a point and you try to fold it up from the left, you'll never reach an end!"

-- "Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that. Whenever you want to traverse a list to return something, chances are you want a fold. That's why folds are, along with maps and filters, one of the most useful types of functions in functional programming."

-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list. 
-- Use scanl to answer the question "How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?

sumsOfRoots = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- Note that "scanl1" (not scanl) starts the fold by using the first value as the accumulator, like foldl1, so there's no zero as the head element here.

-- Function application with $ has type:
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x  

-- Whereas normal function application (space) has high precedence, the $ function has the lowest precedence.
-- For example: f g x is the same as (f g) x  -- Left-associative!
-- But f $ g x = f (g x)  -- Right-associative!

-- You can imagine a $ to be sort of the equivalent of writing an opening parentheses and then writing a closing one one the far right of the expression.

-- These are equal:
sum1 = sum (filter (> 10) (map (* 2) [2 .. 10]))
sum2 = sum $ filter (> 10) $ map (* 2) [2 .. 10]

-- One can also use function application to "lift" values to functions
mapped2 = map ($ 3) [(4 +), (10 *), (^ 2), sqrt]
-- [7.0,30.0,9.0,1.7320508075688772]

-- Function composition produces a new function like
-- (f \circ g)(x) == f(g(x))
-- In Haskell, one can do function composition with the "." function, of type:
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- For example, these are equal:
negate1 = map (\x -> negate (abs x)) [5, -3, -6, 7, -3, 2, -19, 24]
negate2 = map (negate . abs) [5, -3, -6, 7, -3, 2, -19, 24]

-- "If you want to rewrite an expression with a lot of parentheses by using function composition, you can start by putting the last parameter of the innermost function after a $ and then just composing all the other function calls, writing them without their last parameter and putting dots between them."

ugly = replicate
    100
    (product (map (* 3) (zipWith max [1, 2, 3, 4, 5] [4, 5, 6, 7, 8])))

pretty =
    replicate 100  -- This is applied last
        . product  -- This is applied third
        . map (* 3)  -- This is applied second
        . zipWith max [1, 2, 3, 4, 5]  -- This is applied first
        $ [4, 5, 6, 7, 8]  -- This is the "input" list

-- One can also use composition to write point-free function definitions:
fn = ceiling . negate . tan . cos . max 50

-- "Making long chains of function composition is discouraged, although I plead guilty of sometimes being too composition-happy. The prefered style is to use let bindings to give labels to intermediary results or split the problem into sub-problems and then put it together so that the function makes sense to someone reading it instead of just making a huge composition chain."

-- Four ways to write the oddSquareSum
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))
oddSquareSum' = sum $ takeWhile (< 10000) $ filter odd $ map (^ 2) [1 ..]
oddSquareSum'' = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]
oddSquareSum''' =
    let oddSquares = filter odd $ map (^ 2) [1 ..]
        belowLimit = takeWhile (< 10000) oddSquares
    in  sum belowLimit
