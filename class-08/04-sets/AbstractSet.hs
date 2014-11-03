module AbstractSet where

class AbstractSet a where
  empty :: (Eq t, Ord t) => a t
  isEmpty :: (Eq t, Ord t) => a t -> Bool
  insert :: (Eq t, Ord t) => a t -> t -> a t
  exists :: (Eq t, Ord t) => a t -> t -> Bool
  toList :: (Eq t, Ord t) => a t -> [t]
  fromList :: (Eq t, Ord t) => [t] -> a t
