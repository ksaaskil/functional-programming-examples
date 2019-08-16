-- Chapter 8: http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- Enter interactive mode with `stack ghci`
-- and load the module with `:l StartingOut`
module MakingOurOwnTypesAndTypeclasses
    ( Shape'(Rectangle', Circle')
    )
where

import qualified Data.Map                      as Map

-- Use the data keyword to define a type.
-- The parts after the = are value constructors. They specify the different values that this type can have.
data Bool' = False' | True'

-- Here, the Circle value constructor has three fields, which take floats. So when we write a value constructor, we can optionally add some types after it and those types define the values it will contain. Here, the first two fields are the coordinates of its center, the third one its radius. The Rectangle value constructor has four fields which accept floats. The first two are the coordinates to its upper left corner and the second two are coordinates to its lower right one.
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- Value constructors are actually functions that ultimately return a value of a data type.
-- The type of Circle is: "Circle :: Float -> Float -> Float -> Shape"

surface :: Shape -> Float
surface (Circle _ _ r         ) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

-- Note that Circle is not a type, Shape is. Circle is a value constructor and it can be pattern matched.

-- Using surface:
surfaceArea = surface $ Circle 10 20 10

-- Value constructors are functions so we can map and curry them. For example:

circles = map (Circle 10 20) [4, 5, 6, 6]

-- Intermediate data types:
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) =
    abs (x2 - x1) * abs (y2 - y1)

surfaceArea' = surface' (Rectangle' (Point 0 0) (Point 100 100))

nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x + a) (y + b)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) a b =
    Rectangle' (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))


-- Example person using the record syntax

data Person = Person { firstName :: String
    , lastName:: String
    , age:: Int
    , height:: Float
    , phoneNumber :: String
    , flavor:: String } deriving (Show, Eq, Read)

guy = Person { firstName   = "Buddy"
             , lastName    = "Finklestein"
             , age         = 43
             , height      = 184.2
             , phoneNumber = "526-2928"
             , flavor      = "Chocolate"
             }

-- Type constructors can take types as parameters to produce new types.
data Maybe' a = Nothing' | Just' a

-- The a here is the type parameter. Because Maybe takes a type parameter, it's called a type constructor. It may end up producing a Maybe Int, Maybe Car, etc. 

-- It's a very strong convention in Haskell to never add typeclass constraints in data declarations like this:
-- data (Ord k) => Map k v = ...  
-- This would require functions like "toList :: Map k a -> [(k, a)]" to have the typeclass constraint
-- "toList :: (Ord k) => Map k a -> [(k, a)]" even though they don't care k is an Ord.

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

-- It's very important to distinguish between the type constructor and the value constructor. When declaring a data type, the part before the = is the type constructor and the constructors after it (possibly separated by |'s) are value constructors. The vector type constructor takes one parameter whereas the value constructor takes three.

vectorSum = Vector 3 5 8 `vplus` Vector 9 2 5

-- Typeclass is a sort of an interface that defines some behavior. **A type can be made an instance of a typeclass if it supports that behavior**. Example: the Int type is an instance of the Eq typeclass because the Eq typeclass defines behavior for stuff that can be equated. 

-- Unlike from classes in languages like Java, one does not make data from typeclasses. Instead, one makes the data type and then thinks about how it can act. An `Int` can be equatded, so it should be an instance of the `Eq` typelass.

-- We can read a person from string by deriving from Read typeclass
readPerson =
    read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

-- One can use algebraic data types to make enumerations having Enum and Bounded typeclasses:
-- Note that all value constructors here are "nullary", i.e., take no parameters.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

comparison = Monday `compare` Wednesday -- LT

firstDay = minBound :: Day

days = [Thursday .. Sunday]
allDays = [minBound .. maxBound] :: [Day]

-- Type synomyms give types different names like this
type String' = [Char]

-- Add some phone book helpers
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
    [ ("betty"  , "555-2938")
    , ("bonnie" , "452-2928")
    , ("patsy"  , "493-2928")
    , ("lucille", "205-2928")
    , ("wendy"  , "939-8282")
    , ("penny"  , "853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Parameterized type synonym (a type constructor)
type AssocList k v = [(k, v)]

-- Remember: values can only have types that are concrete types!

-- Definition of Either
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

-- Locker example
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing ->
        Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]

lookedUp = lockerLookup 101 lockers -- Right "JAH31"

-- Recursive data structures are data structures whose definition refers to themselves. For example:

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- list = 5 `Cons` (4 `Cons` Empty)

-- Using fixity declaration:
infixr 5 :-:  -- Right-associative with "bind" 5, infix because has only special chars
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

list = 3 :-: 4 :-: Empty

-- Custom concatenation
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty      .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Notice the pattern match on :-:. This works because :-: is a constructor for the list type, just like : is a constructor the standard list type and can be pattern matched.

a = 3 :-: 4 :-: 5 :-: Empty
b = 6 :-: 7 :-: Empty
c = a .++ b

-- Binary search tree
-- A tree is either an empty tree or it's an element that contains some value and two trees.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) | x == a = Node x left right
                                 | x < a  = Node a (treeInsert x left) right
                                 | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right) | x == a    = True
                               | x < a     = treeElem x left
                               | otherwise = treeElem x right

nums = [8, 2, 4, 5, 7, 10]
numsTree = foldr treeInsert EmptyTree nums

-- Typeclasses 102

-- This is how the Eq (type)class is defined in Prelude:
class Eq' a where
    (.==) :: a -> a -> Bool
    (./=) :: a -> a -> Bool
    x .== y = not (x ./= y)  -- Mutual recursion
    x ./= y = not (x .== y)

-- Here a is the type variable. How to define instances of the class? Here's an example:

data TrafficLight = Red | Yellow | Green

instance Eq' TrafficLight where
    Red    .== Red    = True  -- Overwrite only (==): the minimal complete definition
    Green  .== Green  = True
    Yellow .== Yellow = True
    _      .== _      = False

-- Class is for defining new typeclasses and instance is for making types instances of typeclasses.
-- Also define instance of Show TrafficLight:
instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

-- One can also subclass, for example subclassing Num from Eq is essentially a **class constraint on a class declaration**.
class (Eq' a) => Num' a where
    someNumBehaviour :: a -> a

-- To define instances on type constructors, one needs to use a concrete type like (Maybe m):
{- instance (Eq' m) => Eq' (Maybe m) where
    Just x  == Just y  = x == y  -- Needs class constraint on m
    Nothing == Nothing = True
    _       == _       = False -}

-- Most of the times, class constraints in class declarations are used for making a typeclass a subclass of another typeclass and class constraints in instance declarations are used to express requirements about the contents of some type. For instance, here we required the contents of the Maybe to also be part of the Eq typeclass.

-- Use `:info Eq'` to see what the instances of a typeclass are.

-- Custom YesNo typeclass representing "truthy" values.
class YesNo a where
    yesno :: a -> Bool

-- Typeclass instance on concrete Int type
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno = id  -- From standard library, identity

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True

emptyListTruthy = yesno $ length []  -- False
stringTruthy = yesno "haha"
justTruthy = yesno (Just 0)
trueTruthy = yesno True

-- Function that works on yesno instances
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult else noResult

-- Functor typeclass gives the fmap function on all its instances:
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

-- Note that f is NOT a concrete type but a type constructor. fmap takes a function from one type to another and a functor applied with one type and returns a functor applied with another type.

-- Functor instance on list can just use the list map:

instance Functor' [] where  -- Here [] is a type constructor (list), not concrete type
    fmap' = map

fmapped = fmap' (+ 3) [1, 2, 3]

-- Functor instance on Maybe:
instance Functor' Maybe where
    fmap' f (Just x) = Just (f x)
    fmap' _ Nothing  = Nothing

fmappedMaybe = fmap' (* 2) (Just 3)

-- Functor instance for our binary tree
instance Functor Tree where
    fmap _ EmptyTree           = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

fmappedTree = fmap (* 4) numsTree

-- What about Either, it takes two type parameters and Functor only wants one?
-- Simple, feed in one of the type parameters. That's because if the functor is for Either a
-- the type signature becomes "(b -> c) -> (Either a) b -> (Either a) c" which makes a lot of sense:
-- we only want to map over Right.
instance Functor' (Either a) where
    fmap' f (Right x) = Right (f x)
    fmap' f (Left  x) = Left x

fmappedEither = fmap' (+ 2) (Right 10 :: Either String Int)

-- Kinds are "labels" on types, kind of like type of type. Kinds can be inspected with `:k` in ghci:
-- ghci> :k Int
-- Int :: *
-- ghci> :k Maybe
-- Maybe :: * -> *  -- Type constructor takes one concrete type and returns a concrete type.

-- Types are the labels of values and kinds are the labels of types.

-- What goes on here?
class Tofu t where
    tofu :: j a -> t a j

-- ja is used as the type of a value so "j a" must have kind *.
-- Therefore, j must have kind * -> *. t takes two types and must produce a concrete value
-- so it must have kind * -> (* -> *) -> *. So it takes a concrete type, a type constructor that takes one concrete type, and produces a concrete type.

-- Example of a type with that kind:
data Frank a b =  Frank { frankField :: b a } deriving (Show)

frank = Frank { frankField = Just "HAHA" }
-- ghci> frank :: Frank [Char] Maybe

instance Tofu Frank where
    tofu x = Frank x

tofued = tofu (Just 'a') :: Frank Char Maybe  
