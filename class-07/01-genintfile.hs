{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import Data.List
import System.Random
import System.Environment

split' :: [a] -> Int -> Int -> [[a]]
split' xs 0 k = []
split' xs n k = (take k xs) : (split' (drop k xs) (n-1) k)

l2s :: Show a => [a] -> String
l2s ls = unwords $ map show ls

createFile :: RandomGen generator => generator -> Int -> Int -> Int -> Int -> FilePath -> IO ()
createFile gen from to numbers rows dst = writeFile dst $ unlines $ map l2s $ genRows
  where
    genRows = split' (randomRs (from, to) gen) rows numbers

{- 01-genintfile <file> <от> <до> <чисел> <строк> -}
main = do
  [file, from, to, numbers, rows] <- getArgs
  g <- newStdGen
  createFile g (read from) (read to) (read numbers) (read rows) file
