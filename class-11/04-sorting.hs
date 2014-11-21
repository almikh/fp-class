{-
  Пользуясь средствами монады ST, запрограммировать сортировку массива тремя любыми методами.
  -}

import Data.STRef
import Control.Monad
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.Array.MArray


swapElems :: Ix i => i -> i -> STArray s i e -> ST s ()
swapElems i j arr = do
  vi <- readArray arr i
  vj <- readArray arr j
  writeArray arr i vj
  writeArray arr j vi

partition' :: Ord e => STArray s Int e -> Int -> Int -> ST s Int
partition' arr beg end = do
  marker <- newSTRef beg
  forM_ [beg .. end] $ \i -> do
    ai <- readArray arr i
    lastVal <- readArray arr end
    when (ai<=lastVal) $ do
      temp <- readSTRef marker
      swapElems i temp arr
      writeSTRef marker $! temp + 1
  readSTRef marker

quickSort' :: Ord e => STArray s Int e -> Int -> Int -> ST s ()
quickSort' arr start end = do
  when (start<end) $ do
    pivot <- partition' arr start end
    quickSort' arr start (pivot-2)
    quickSort' arr (pivot) end


quickSort :: (Ord a) => [a] -> [a]
quickSort xs = elems $ runSTArray $ do
  arr <- newListArray (0, length xs - 1) xs
  quickSort' arr 0 (length xs - 1)
  return arr

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = elems $ runSTArray $ do
  arr <- newListArray (0, length xs - 1) xs
  forM_ [0 .. (length xs - 2)] $ \i ->
    forM_ [0 .. (length xs - 2 - i)] $ \j -> do
      a <- readArray arr j
      b <- readArray arr (j+1)
      when (a>b) $ swapElems j (j+1) arr
  return arr

selectionSort :: (Ord a) => [a] -> [a]
selectionSort xs = elems $ runSTArray $ do
  arr <- newListArray (0, length xs - 1) xs
  forM_ [0 .. (length xs - 1)] $ \i ->
    forM_ [(i+1) .. (length xs - 1)] $ \j -> do
      ai <- readArray arr i
      aj <- readArray arr j
      when (ai>aj) $ swapElems i j arr
  return arr

testSelectionSort = selectionSort [1, 5, 42, 89, 52, 3] == [1, 3, 5, 42, 52, 89]
testBubbleSort = bubbleSort [1, 5, 42, 89, 52, 3] == [1, 3, 5, 42, 52, 89]
testQuickSort = quickSort [1, 5, 42, 89, 52, 3] == [1, 3, 5, 42, 52, 89]
