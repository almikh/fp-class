import System.Environment
import Data.Monoid
import Data.Maybe

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData = map (\x -> if x=="-" then Nothing else Just (read x :: Int)) . lines

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay xs = (take 5 xs) : dataByDay (drop 5 xs)

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

ignoreBadDays :: [SensorData] -> [SensorData]
ignoreBadDays = filter (any isJust)

minData1 :: Bool -> [SensorData] -> Int
minData1 needFirst = fromJust . minimum . filter isJust . map select . ignoreBadDays
  where
    select
      | needFirst==True = getFirst . mconcat . map First
      | otherwise = getLast . mconcat . map Last

{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}

minData2 :: Bool -> [SensorData] -> Int
minData2 needSum = minimum . map select . ignoreBadDays
  where
    select
      | needSum==True = getSum . mconcat . map (Sum . fromJust) . filter (isJust)
      | otherwise = getProduct . mconcat . map (Product . fromJust) . filter (isJust)

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct
  deriving Eq

minData :: SensorTask -> [SensorData] -> Int
minData st
  | st==NeedFirst = minData1 True
  | st==NeedLast = minData1 False
  | st==NeedSum = minData2 True
  | otherwise = minData2 False

{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}
stat :: Int -> Int -> [SensorData] -> Int
stat 1 _ = length . filter (getAll . mconcat . map All . map isNothing)
stat 2 _ = length . filter (getAll . mconcat . map All . map isJust)
stat 3 _ = length . filter (getAny . mconcat . map Any . map isJust)
stat 4 num = length . filter filterFunc
  where filterFunc x = (getSum . mconcat . map (Sum . fromJust) . filter (isJust)) x > num
stat 5 num = length . filter filterFunc
  where filterFunc x = (getProduct . mconcat . map (Product . fromJust) . filter (isJust)) x > num
stat 6 num = length . filter (\x -> (getFirst . mconcat . map First) x > Just num)
stat 7 num = length . filter (\x -> (getLast . mconcat . map Last) x > Just num)

main = do
  fname <- head `fmap` getArgs
  let num = 44
  sData <- stat 1 num `fmap` dataByDay `fmap` getData `fmap` readFile fname
  print sData
  sData <- stat 2 num `fmap` dataByDay `fmap` getData `fmap` readFile fname
  print sData
  sData <- stat 3 num `fmap` dataByDay `fmap` getData `fmap` readFile fname
  print sData
  sData <- stat 4 num `fmap` dataByDay `fmap` getData `fmap` readFile fname
  print sData
  sData <- stat 5 num `fmap` dataByDay `fmap` getData `fmap` readFile fname
  print sData
  sData <- stat 6 num `fmap` dataByDay `fmap` getData `fmap` readFile fname
  print sData
  sData <- stat 7 num `fmap` dataByDay `fmap` getData `fmap` readFile fname
  print sData
