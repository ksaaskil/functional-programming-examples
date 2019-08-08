-- Chapter 3: http://learnyouahaskell.com/types-and-typeclasses
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module TypesAndTypeClasses
    ()
where

-- Haskell has a static type system.
-- Use `:t` in `ghci` to tell a type of a variable.

-- Read this as "a has type of Char"
-- ghci> :t 'a'
-- 'a' :: Char

-- Double quotes mark a "String"
-- ghci> :t "a"
-- "a" :: [Char]

-- Functions can be given explicit type declarations as follows
removeNonUpperCase :: String -> String
removeNonUpperCase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]

-- Curried function can be defined as follows
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Type variables are a bit like generics in other languages. See them in action:
-- ghci> :t head
-- head :: [a] -> a
-- The above says that `head` is a polymorphic function that can be used on a list of any type
-- and returns an element of that type.

-- A typeclass is a kind of interface that defines behaviour. Consider the "==" functions:
-- ghci> :t (==)
-- (==) :: Eq a => a -> a -> Bool

-- Note that "==" contains only special characters so it's considered infix by default.
-- Its type can be inspected by wrapping it in parentheses.

-- Above, everything before `<==` is called a class constraint. The type is read as follows:
-- "(==) takes any two values that are of the same type and returns a Bool. The type of those two values must be a member of the Eq class"
-- Here the `Eq` typeclass provides an interface for testing equality.

-- Another example with `elem`: 
-- ghci> :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- Here `t a` is a foldable (like list) containing values of type `a`.

-- Basic type classes:

-- `Eq` is used for types that support testing for equality. Its members implement the functions `==` and `/=`

-- `Ord` if for types that have ordering
-- ghci> :t (>)
-- (>) :: Ord a => a -> a -> Bool
-- ghci> :t compare
-- compare :: Ord a => a -> a -> Ordering

-- Members of `Show` can be presented as strings with the `show` function:
-- ghci> :t show
-- show :: Show a => a -> String

-- Members of `Read` can be constructed from a String with the `read` function
-- ghci> :t read
-- read :: Read a => String -> a
-- When reading an ambiguous type like `read "4"`, you can use explicit type annotations to help Haskell infer the type:
-- ghci> read "4" :: Int  

-- Members of `Bounded` have an upper and lower bound that one can access with, e.g., `minBound`
-- ghci> :t minBound
-- minBound :: Bounded a => a

-- Note that `minBound` does not take input arguments, "Bounded a" is the class constraint.
-- `minBound` is a kind of polymorphic constant.

-- ghci> minBound :: Int
-- -9223372036854775808

-- `Num` is a numeric typeclass for beings able to act like numbers.
-- ghci> :t 20
-- 20 :: (Num p) => p

-- Numbers are also "polymorphic constants" able to act like any type that's a member of the `Num` type class:

a = 20 :: Int
b = 20 :: Double

-- One can use functions like `fromIntegral` to map an Integral (Int or Integer) to a `Num`:
-- ghci> :t fromIntegral
-- fromIntegral :: (Integral a, Num b) => a -> b

c = fromIntegral (length [1, 2, 3, 4]) + 3.2
