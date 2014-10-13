{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.IO
import System.Environment
import System.Directory
import Data.Char

{- parsing -}
{- --l <file> -}
parse "--l" (file:[]) = do
	contents <- readFile file
	putStrLn $ show $ length $ lines contents

{- --add [-t|-h] <string> <file> -}
parse "--add" (mode:str:file:[]) = do
	if mode=="-t" then appendFile file str
	else if mode=="-h" then do
		contents <- readFile file
		writeFile "temp.txt" $ unlines $ str : (lines contents)
		renameFile "temp.txt" file
	else undefined

{- --to-upper <file> -}
parse "--to-upper" (file:[]) = do
		contents <- readFile file
		writeFile "temp.txt" $ foldr (\x acc -> if isLetter x then (toUpper x):acc else x:acc) [] contents
		renameFile "temp.txt" file

parse _ _ = undefined

help = [
	"Program options:",
	"",
	" --l <file>                         lines in file",
	" --add [-t|-h] <string> <file>      add string to tail/head in file",
	" --to-upper <file>                  chars from file to upper register"]

{- main -}
main = do
	cmd:params <- getArgs
	if cmd=="-h" then putStrLn $ unlines help
	else parse cmd params
