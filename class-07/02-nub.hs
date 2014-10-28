{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных.
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list = length . group . sort

unique' :: (Ord a, Eq a) => Seq.Seq a -> Seq.Seq a
unique' s
  | Seq.null s = Seq.empty
  | otherwise = l Seq.<| (unique' (Seq.dropWhileL (==l) s'))
    where (l Seq.:< s') = Seq.viewl s

nub_seq :: (Ord a, Eq a) => Seq.Seq a -> Int
nub_seq s = Seq.length $ unique' $ Seq.sort s

nub_arr :: Array Int Int -> Int
nub_arr a = nub_list $ elems a

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
