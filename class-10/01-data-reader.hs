{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
import System.Environment
import Control.Monad
import Data.Monoid
import qualified Data.List as DL

data Record = Record {
                name :: String,
                age :: Int,
                group :: String
              } deriving (Eq)

instance Show Record where
  show rec = (name rec) ++ ", " ++ (show (age rec)) ++ ", " ++ (group rec) ++ "gr."

instance Ord Record where
  r1 `compare` r2 = (name r1) `compare` (name r2)
  r1 <= r2 = (name r1) <= (name r2)

toRecord :: String -> Record
toRecord s = Record n (read a :: Int) g
    where [n, a, g] = words s

fromRecord :: Record -> String
fromRecord (Record n a g) = n ++ " " ++ (show a) ++ " " ++ g

readFromFile :: String -> [Record]
readFromFile = map toRecord . lines

cat :: [Record] -> [Record] -> [Record]
cat r1 r2 = DL.sort $ r1 ++ r2

{- 01-data-reader <file1> <file2> -}
main =
  (unlines . map fromRecord) `liftM` (cat `liftM` readFstFile `ap` readSndFile) >>= writeFile "dest.txt"
    where {- так просто главная строчка короче -}
      loadList file = readFromFile `fmap` readFile file
      readFstFile = (head `liftM` getArgs) >>= loadList
      readSndFile = (last `liftM` getArgs) >>= loadList
