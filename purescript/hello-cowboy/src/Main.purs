module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Math (pow)
import Data.Int (toNumber)

data SendingMethod 
    = Email String
    | Address { street  :: String, 
                city    :: String, 
                country :: String }

exampleSendingMethod :: SendingMethod
exampleSendingMethod = Email "kimmo@meeshkan"

maybeVal :: Maybe String
maybeVal = Just "some string"

otherMaybeVal :: Maybe String
otherMaybeVal = maybeVal >>= (\s -> Just (s <> "some other string"))

showTheValue :: Maybe Number -> String
showTheValue value =
    case value of
        Nothing -> "There is no value"
        Just value' -> "The value is: " <> show value'

showSendingMethod :: SendingMethod -> String
showSendingMethod sendingMethod = 
    case sendingMethod of
        Email email -> "Sent by mail to: " <> email
        Address address -> "Sent to an address"

square :: Number -> Number
square number = pow number $ toNumber 2

numbers :: Array Number
numbers = map toNumber [2, 5, 8]

squared :: Array Number
squared = map square numbers
-- [4,25,64]

-- Data types and pattern matching
data Tuple a b = Tuple a b  -- Tuple in LHS is a type constructor, in RHS a value constructor
data Currency = Int  -- Type alias, improves readability
data Money = Money { currency :: Currency }  -- Record type

data Person = Person { name :: String, age :: Int }

-- Pattern matching on the value
showPerson :: Person -> String
showPerson (Person o) = o.name <> ", aged " <> show o.age

examplePerson :: Person
examplePerson = Person { name: "Bonnie", age: 26 }

examplePersonShown :: String
examplePersonShown = showPerson examplePerson

-- newtype declarations
newtype Percentage = Percentage Number

-- Typeclass instance of show for newtype
instance showPercentage :: Show Percentage where
  show (Percentage n) = show n <> "%"

-- Polymorphic types 
identity :: forall a. a -> a
identity x = x

-- Type aliases
type Foo = { foo :: Number, bar :: Number }

foo :: Foo
foo = { foo: toNumber 1, bar: toNumber 2 }

-- Create an alias for a polymorphic record with the same shape
type Bar a = { foo :: a, bar :: a }  -- Bar Number is the same as Foo

main :: Effect Unit
main = do
  log "Hello sailor!"
