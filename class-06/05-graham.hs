{- нужный модуль лежит в другой ветке, поэтому, чтобы не ломать голову, просто вставлю сюда нужный код -}
import Data.List
import System.IO
import System.Environment

data Point = Point { x :: Double, y :: Double }
  deriving (Show, Eq)

data Direction = RTOL | RTOR | NR
  deriving (Show, Eq)

fromPair :: (Double, Double) -> Point
fromPair (x, y) = Point x y

toPair :: Point -> (Double, Double)
toPair (Point x y) = (x, y)

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

dir :: Point -> Point -> Point -> Direction
dir a b c
  | ux*vy - uy*vx > 0 = RTOL
  | ux*vy - uy*vx < 0 = RTOR
  | otherwise = NR
    where
      (ux, uy) = (x b - x a, y b - y a)
      (vx, vy) = (x c - x a, y c - y a)

graham_scan :: [Point] -> [Point]
graham_scan = final . sort' . getKeyPoint
  where
    getKeyPoint points = foldl func (head points, []) (tail points)
      where func ((Point x y), others) (Point nx ny) = if nx<x then ((Point nx ny), (Point x y):others) else ((Point x y), (Point nx ny):others)
    {- сортировка в порядке 'левизны' относительно ключевой точки; (!) если одинаково, то по дистанции до нее -}
    sort' (key, others) = (key, sortBy sortFunc others)
      where
        sortFunc p pp
          | dir key p pp == RTOL = LT
          | dir key p pp == NR = if dist key p < dist key pp then LT else GT
          | otherwise = GT
    {- удаление лишних точек -}
    final (key, x:others) = (foldl validationFunc (x:key:[]) others)
      where
        removeInvalid (x:y:z:stack)
          | dir x z y == RTOR = removeInvalid (x:z:stack)
          | otherwise = (x:y:z:stack)
        validationFunc acc p = removeInvalid (p:acc)

{-
   Дописать к реализованному ранее алгоритму Грэхема основную программу, которая принимает
   на вход файл со списком точек и создаёт файл со списком точек, образующих выпуклую оболочку.

   Для отыскания пути к импортируемому модулю следует использовать параметр -i командной строки
   (для ghc и ghci), например:

     $ ghc 05-graham.hs  -o graham -i../class-05/3-Graham_scan/
-}

{-
example: 05-graham inp.txt out.txt
inp.txt:
  (1.0, 2.0)
  (3.0, 4.0)
  ...
-}


main = do
  [inp, out] <- getArgs
  contents <- readFile inp
  writeFile out $  unlines $ map show $ map toPair $ graham_scan $ getPoints contents
    where getPoints c = map fromPair $ map (\x -> read x :: (Double, Double)) $ lines c
