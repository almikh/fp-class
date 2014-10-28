{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile file = do
  contents <- readFile file
  return (map (\x -> (read x) :: Int) $ words contents)


solve :: [[Int]] -> (Int, [Int])
solve s = (length result, result)
  where
    result = Set.toAscList $ Set.unions $ map (Set.fromList) s

main = getArgs >>= mapM readNumFile >>= print.solve
