{-
  Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
  имя поля=значение
  Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
  являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
  и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
  указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
  Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
  Организовать доступ к параметрам конфигурационного файла средствами монады Reader.
  -}
import System.Environment
import Control.Monad.Reader

data ConfElem =
  ConfElem String Int deriving Show

type Config = [ConfElem]

process :: Int -> ConfElem -> Int
process lhs (ConfElem "summand" rhs) = lhs + rhs
process lhs (ConfElem "multiplier" rhs) = lhs * rhs
process lhs (ConfElem "divisor" rhs) = lhs `div` rhs

loadConfig :: String -> Config
loadConfig = map (\xs -> ConfElem (getName xs) (getVal xs)) . lines
  where
    getName = takeWhile (/='=')
    getVal = read . tail . dropWhile (/='=')

work :: Int -> Reader Config Int
work number = do
  e <- ask
  return $ foldl process number e

main = do
  [conf, numbs] <- getArgs
  contents <- readFile conf
  numbers <- readFile numbs
  let config = loadConfig contents
  mapM_ (\num -> (print . runReader (work num)) config) $ map read $ lines numbers
