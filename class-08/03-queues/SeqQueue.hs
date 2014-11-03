module SeqQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue
import qualified Data.Sequence as Seq

newtype Queue t = SeqQueue (Seq.Seq t)

instance AbstractQueue Queue where
  empty = SeqQueue Seq.empty
  isEmpty (SeqQueue xs) = Seq.null xs
  enqueue (SeqQueue xs) x = SeqQueue (xs Seq.|> x)
  dequeue (SeqQueue xs) = (x, SeqQueue rhs)
    where (x Seq.:< rhs) = Seq.viewl xs
