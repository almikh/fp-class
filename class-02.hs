-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms t = (h, m, s)
	where 
		h = t `div` 3600
		m = (t `mod` 3600) `div` 60
		s = t `mod` 60
	

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*3600 + m*60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt (sqr (x1-x2) + sqr(y1-y2))
	where 
		sqr x = x*x

-- triangle :: ??? -> (Double, Double)
triangle :: Point -> Point -> Point -> (Double, Double)
triangle a b c = (p, s)
	where
		l1 = distance a b
		l2 = distance b c
		l3 = distance c a
		p = l1 + l2 + l3
		pp = (l1 + l2 + l3) / 2
		s = sqrt $ pp * (pp-l1) * (pp-l2) * (pp-l3)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = if even x then 1 + nEven xs else nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems xs = [x*2 | x <- xs]

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if odd x then x : fltOdd xs else fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
rmNegative :: Integral a => [a] -> [a]
rmNegative [] = []
rmNegative (x:xs) = if x>=0 then x : rmNegative xs else rmNegative xs

-- б) увеличить элементы с чётными значениями в два раза;
dblEven :: Integral a => [a] -> [a]
dblEven [] = []
dblEven (x:xs) = if even x then (2*x) : dblEven xs else x : dblEven xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
rearr_elem :: Integral a => [a] -> [a]
rearr_elem [] = []
rearr_elem (x:[]) = []
rearr_elem (x:xs) = (head xs) : x : rearr_elem(tail xs)

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y) : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_pair :: [Integer] -> [Integer] -> [(Integer,Integer)]
combine_pair [] ys = []
combine_pair xs [] = []
combine_pair (x:xs) (y:ys) = (x, y) : combine_pair xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
first_n' :: Int -> [Int]
first_n' n = reverse [1 .. n]
-- б) в порядке возрастания.
first_n :: Int -> [Int]
first_n n = [1 .. n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
afterEach :: Num a =>  [a] -> a -> [a]
afterEach [] e = []
afterEach (x:xs) e = if length xs == 1 then x : e : xs
					else x : e : afterEach xs e

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
split' :: Eq a =>  [a] -> ([a], [a])
split' [] = ([], [])
split' [x] = ([x], [])
split' xs = (left, drop (length left) xs)
	where 
		def_left (x1:x2:xs') = if x1==x2 then x1 : (
									if length xs'>1 then def_left (x2:xs')
									else [x2]) 
								else [x1]
		left = def_left xs

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
nth_elem :: Num a =>  [a] -> Int -> a
nth_elem [] n = error "error: nth_elem"
nth_elem (x:xs) 0 = x
nth_elem (x:xs) n = nth_elem xs (n-1)

-- б) Eq a => [a] -> a -> Bool
exist' :: Eq a => [a] -> a -> Bool
exist' [] y = False
exist' (x:xs) y = if x==y then True else exist' xs y

-- в) [a] -> Int -> [a]
take' :: [a] -> Int -> [a]
take' [] n = []
take' xs 0 = []
take' (x:xs) n = x : take' xs (n-1)

-- г) a -> Int -> [a]
repeat' :: a -> Int -> [a]
repeat' x 0 = []
repeat' x n = x : repeat' x (n-1)

-- д) [a] -> [a] -> [a]
intersect' :: Eq a =>  [a] -> [a] -> [a]
intersect' xs [] = []
intersect' [] ys = []
intersect' (x:xs) ys = if x `elem` ys then x : intersect' xs ys else intersect' xs ys

-- е) Eq a => [a] -> [[a]]
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' xs = cur : group' (drop (length cur) xs)
	where 
		next_group (x1:x2:xs') = if x1==x2 then 
									x1 : (if null xs' then [x2] else next_group (x2:xs')) 
								else [x1]
		cur = next_group xs
-- group' [1, 1, 1, 2, 3, 3, 4, 5, 5, 6]
-- > [[1, 1, 1], [2], [3, 3], [4], [5, 5], [6]]
		
-- ж) [a] -> [(Int, a)]
zip' :: Eq a => [a] -> [(Int, a)]
zip' xs = map func $ group' xs
	where func xs = (length xs, head xs)

-- з) Eq a => [a] -> [a]
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : (nub' (erase' x xs))
	where 
		erase' y [] = []			  
		erase' y (x:xs) = if y==x then (if null xs then [] else erase' y xs)
						  else x : (if null xs then [] else erase' y xs)