-- Example abstract data type
-- https://www.haskell.org/tutorial/modules.html
module TreeADT
    ( Tree
    , leaf
    , branch
    , cell
    , left
    , right
    , isLeaf
    )
where

data Tree a             = Leaf a | Branch (Tree a) (Tree a)

leaf = Leaf
branch = Branch
cell (Leaf a) = a
left (Branch l r) = l
right (Branch l r) = r
isLeaf (Leaf _) = True
isLeaf _        = False
