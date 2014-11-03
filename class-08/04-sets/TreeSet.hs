module TreeSet (Set, empty, insert, exists, isEmpty) where

import AbstractSet

data Tree a =
  EmptyTree |
  Node a (Tree a) (Tree a) deriving (Eq)

isEmpty' :: (Ord a) => Tree a -> Bool
isEmpty' EmptyTree = True
isEmpty' _ = False

exists' :: (Ord a) => Tree a -> a -> Bool
exists' EmptyTree e = False
exists' (Node val lt rt) e
  | e == val = True
  | e < val = exists' lt e
  | otherwise = exists' rt e

insert' :: (Ord a) => Tree a -> a -> Tree a
insert' EmptyTree e = Node e EmptyTree EmptyTree
insert' (Node val lt rt) e
  | e < val = (Node val (insert' lt e) rt)
  | otherwise = (Node val lt (insert' rt e))

insertIfNotExists' :: (Ord a) => Tree a -> a -> Tree a
insertIfNotExists' EmptyTree e = Node e EmptyTree EmptyTree
insertIfNotExists' (Node val lt rt) e
  | e == val = (Node val lt rt)
  | e < val = (Node val (insertIfNotExists' lt e) rt)
  | otherwise = (Node val lt (insertIfNotExists' rt e))

toList' :: (Ord a) => Tree a -> [a]
toList' EmptyTree = []
toList' (Node val lt rt) = [val] ++ (toList' lt) ++ (toList' rt)

newtype Set a = TreeSet (Tree a)

instance AbstractSet Set where
  empty = TreeSet EmptyTree
  isEmpty (TreeSet tree) = isEmpty' tree
  insert (TreeSet tree) e = TreeSet (insertIfNotExists' tree e)
  exists (TreeSet tree) e = exists' tree e
  toList (TreeSet tree) = toList' tree
  fromList [] = empty
  fromList (x:xs) = insert (fromList xs) x
