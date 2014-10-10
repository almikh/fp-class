{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where
import Data.List

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point { x :: Double, y :: Double }
  deriving (Show, Eq)

{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = RTOL | RTOR | NR
  deriving (Show, Eq)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

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

directions :: [Point] -> [Direction]
directions points = map dir' $ group' points
  where
    group' xs
      | length xs==3 = [xs]
      | otherwise = (take 3 xs) : group' (tail xs)
    dir' (x:y:z:[]) = dir x y z


{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

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
  5. Приведите несколько примеров работы функции graham_scan.
-}

graham_scan_test1 = graham_scan [Point 0 0, Point 1 1, Point 0 2, Point 2 0, Point 2 2]
graham_scan_test2 = graham_scan [Point 0 0, Point 1 1, Point 0 2, Point 2 0, Point 2 2, Point 3 5]
graham_scan_test3 = graham_scan [Point 0 0, Point 1 1, Point 0 2, Point 4 0, Point 2 0, Point 2 2]
