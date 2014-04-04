-- File: BinaryTree.hs
-- Date: Oct 28, 2011
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Descpription:

module BinaryTree
       (
         Tree,
         search,
         balanced,
         node,
         leaf
       ) where

-- A Tree reprents a binary tree
data Tree a = Empty
             | Node a (Tree a) (Tree a)
             deriving (Show, Eq)

instance Ord a => Ord (Tree a) where
  _ >= Empty = True
  (Node x _ _) >= (Node y _ _) = x >= y
  
  _ <= Empty = True
  (Node x _ _) <= (Node y _ _) = x <= y
  
  _ < Empty = False
  (Node x _ _) < (Node y _ _) = x < y
  
  _ > Empty = True
  (Node x _ _) > (Node y _ _) = x > y

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x ls rs) = Node (f x) (fmap f ls) (fmap f rs)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- node a b c = Node a b c where the ording defined by binary trees is enforced
node :: Ord a => a -> Tree a -> Tree a -> Tree a
node i  Empty Empty = leaf i
node i nd@(Node x _ _) md@(Node y _ _)
  | x <= i && y >= i = Node i nd md
  | otherwise = Empty

balanced :: Ord a => Tree a -> Bool
balanced Empty = True
balanced x@(Node _ ls rs) = let y = x >= ls && x <= rs in
  y `seq` (y && balanced ls && balanced rs)

-- after further thinking decided not to implement a lookup
-- function because of how pointless it would be; reasons
-- being that the lookup would be O(n^2) vs O(n) that 
-- association lists provide. Considered implementing a 
-- searchP :: Ord a => (a -> Bool) -> Tree a -> Maybe a
-- but again could be only implemented in O(n^2) and is
-- pretty much the same idea as lookupTree
--lookupTree :: Ord a => (a -> Bool) -> Tree a -> Maybe a

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ ls rs) = 1 + max (depth ls) (depth rs)

put :: Ord a => a -> Tree a -> Tree a
put i Empty = leaf i
put i (Node x ls rs)
  | i > x = Node x ls (put i rs)
  | i < x = Node x (put i ls) rs
  | otherwise = Node x (Node i ls Empty) rs

-- Assumes a proper binary tree; thatis balanced node = True
search :: Ord a => a -> Tree a -> Bool
search _ Empty = False
search i (Node x ls rs)
  | i > x = search i rs
  | i < x = search i ls
  | otherwise = True
