module ListSet (Set, empty, insert, exists, isEmpty) where

import AbstractSet

newtype Set a = ListSet [a]

instance AbstractSet Set where
  empty = ListSet []
  isEmpty (ListSet xs) = null xs
  insert (ListSet xs) e
    | e `elem` xs = (ListSet xs)
    | otherwise = (ListSet (e:xs))
  exists (ListSet xs) e = e `elem` xs
  toList (ListSet xs) = xs
  fromList [] = empty
  fromList (x:xs) = insert (fromList xs) x
