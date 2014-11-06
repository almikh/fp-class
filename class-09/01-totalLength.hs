import System.Environment

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength = sum . map length

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 _ 0 = Nothing
build1 c n = Just (map (\k -> replicate k c) [1 .. n])

{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций:
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 c n
  | n==0 = Left "(*) n=0"
  | n>100 = Left "(*) n > 100"
  | c=='x' = Left "(*) Роспотребнадзор запрещает создавать строки из символа 'x'"
  | otherwise = Right (map (\k -> replicate k c) [1 .. n])

{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}

main = do
  args@[file, aCh, aNum] <- getArgs
  let num = (read aNum :: Int)
  let ch = head aCh
  contents <- readFile file
  result1 <- (show . totalLength) `fmap` getArgs
  result2 <- (show . totalLength . lines) `fmap` readFile file
  let result3 = totalLength `fmap` (build1 ch num)
  let result31 = totalLength `fmap` (build1 ch num)
  let result32 = totalLength `fmap` (build2 ch num)
  let result31' = totalLength `fmap` (build1 ch 0)
  let result32' = totalLength `fmap` (build2 ch 0)
  putStrLn $ "1): " ++ result1
  putStrLn $ "2): " ++ result2
  putStrLn $ "3.1): " ++ (show result31)
  putStrLn $ "3.2): " ++ (show result32)
  putStrLn $ "3.1'): " ++ (show result31')
  putStrLn $ "3.2'): " ++ (show result32')
