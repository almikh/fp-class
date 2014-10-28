{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import qualified Data.Set as Set

createSet :: FilePath -> IO (Set.Set Int)
createSet fname = do
    contents <- readFile fname
    return $ Set.fromList $ map read $ words contents

main = do
  files <- getArgs
  sets <- mapM createSet files
  let dst = Set.unions sets
  print (Set.size dst, Set.foldl (+) 0 dst)
