{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable    #-}  -- For deriving foldables and stuff for common data types
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
-- http://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html#targetText=makeLenses%20creates%20one%20lens%20per,the%20field%20without%20the%20underscore.&targetText=This%20means%20that%20you%20can,provided%20by%20the%20Haskell%20Prelude.
-- Enter interactive mode with `stack ghci`
module LensTutorial
    ()
where



import           Control.Lens

data Point = Point { _x :: Double, _y :: Double } deriving (Show)
data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

-- Brute-forcing change of field
shiftAtomX' :: Atom -> Atom
shiftAtomX' (Atom e (Point x y)) = Atom e (Point (x + 1) y)

-- Autogenerate lenses with Template Haskell (see the top for the pragma):
$(makeLenses ''Atom)
$(makeLenses ''Point)
-- This creates four lenses of types
-- element :: Lens' Atom String
-- point :: Lens' Atom Point
-- x :: Lens' Point Double
-- y :: Lens' Point Double


shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }

-- Add molecule
newtype Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

-- Define "atoms"
$(makeLenses ''Molecule)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

-- Helpers 

atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
molecule = Molecule { _atoms = [atom1, atom2] }

shiftedMolecule = shiftMoleculeX molecule

-- What is a lens? A lens is a first class getter and setter. `view` is a "get":
viewAtom = view (point . x) atom

-- You could pretend they're defined as follows:
-- data Lens a b = Lens { view :: a -> b, over :: (b -> b) -> (a -> a)}

-- Actual definition is like this:
-- type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
-- The trick here is that we get to pick what Functor we specialize f to and depending on which Functor we pick we get different features.

-- Instead of using Template Haskell, one can use `lens` to build lenses:
-- lens :: (a -> b) -> (a -> b -> a) -> Lens' a b
-- The first argument is the "getter". The second argument is the "setter". Define our own lens:

point' :: Lens' Atom Point
point' = lens _point (\atom newPoint -> atom { _point = newPoint })

-- Without lens:
-- point'' :: Lens' Atom Point
point'' :: Functor f => (Point -> f Point) -> Atom -> f Atom
point'' k atom =
    fmap (\newPoint -> atom { _point = newPoint }) (k (_point atom))

-- Lenses can be combined using function composition.
-- (.) :: Lens' a b -> Lens' b c -> Lens' a c
atomToX :: Lens' Atom Double
atomToX = point . x

-- Getter
viewX :: Atom -> Double
viewX = view atomToX

-- Setter
overX :: (Double -> Double) -> (Atom -> Atom)
overX = over atomToX

increaseXByOne :: Atom -> Atom
increaseXByOne = overX (+ 1)

-- How to consume lenses?
-- view :: Lens' a b -> a -> b
-- over :: Lens' a b -> (b -> b) -> (a -> a)
-- set :: Lens ' a b ->       b  -> (a -> a)  -- Special case of `over`

-- view and over distribute over lens composition:
-- view (lens1 . lens2) = (view lens1) . (view lens2)
-- over (lens1 . lens2) = (over lens1) . (over lens2)

-- Traversals
-- Traversal is a first class getter and setter for an arbitrary number of values. It lets you get all the values it points to as a list and to update the values it points to. Think of it like this:
-- data Traversal' a b = Traversal' { toListOf :: a -> [b], over :: (b -> b) -> (a -> a)}

-- We saw a traverse here:
-- shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

-- Let's bite this into pieces:
atomX :: Lens' Atom Double
atomX = point . x

moleculeX :: Traversal' Molecule Double  -- Defines essentially "over" and "toListOf"
moleculeX = atoms . traverse . atomX

-- Compose with a single shift function:
shift lens = over lens (+ 1)
shiftAtomX'' = shift atomX
shiftMoleculeX'' = shift moleculeX

-- As moleculeX is a Traversal', it has `toListOf` instead of `view`
allX :: Molecule -> [Double]
allX = toListOf moleculeX

-- We're using a special case of Traversal where the Traversable is of type [] (an array).

-- One can derive Functor, Foldable, and Traversable for many data types using `DeriveFoldable` etc.
data Pair a = Pair a a deriving (Show, Functor, Foldable, Traversable)  -- Relies on extensions

pair :: Pair Integer
pair = Pair 3 4

-- Traversing by mapping to a list
traverseToArray :: [Pair Integer]
traverseToArray = traverse (\x -> [x]) (Pair 3 4)

-- Traversing by mapping to a Just
traverseToMaybe :: Maybe (Pair Integer)
traverseToMaybe = traverse (\x -> Just x) (Pair 3 4)

traverseToChildren :: Traversal' (Pair a) a
traverseToChildren = traverse

-- updatePair :: (Integer -> Identity Integer) -> (Pair Integer -> Pair Integer)
-- updatePair pair = over traverse
updatedPair = over traverse (+ 1) (Pair 3 4)

-- Traversals can be composed as lenses can

-- atoms :: Traversal' Molecule [Atom] --- Lens' is a Traversal'
traverseAtoms :: Traversal' Molecule Atom
traverseAtoms = atoms . traverse

traversePoints :: Traversal' Molecule Point
traversePoints = traverseAtoms . point

traverseX :: Traversal' Molecule Double
traverseX = traversePoints . x

-- Set all x coordinates
incrementX :: Molecule -> Molecule
incrementX = over traverseX (+ 3)

-- Get all x coordinates
viewXs :: Molecule -> [Double]
viewXs = toListOf traverseX
