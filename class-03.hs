{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка.
-}

{-
 1. Простейшие задачи на применение функций map и filter.
 1.1 Преобразовать данный список целых чисел следующим образом:
  a) увеличить все его элементы в два раза;
  b) увеличить все его элементы с четными значениями в два раза;
  с) обнулить все его элементы с нечетными значениями;
  d) удалить из него элементы, большие заданного числа k;
  e) отфильтровать его, оставив в списке только отрицательные числа;
  f) удалить из него все положительные чётные числа.
-}

import Data.Char
import Data.List

f11a :: Integral a => [a] -> [a]
f11a = map (*2)

f11b :: Integral a => [a] -> [a]
f11b = map (\x -> if even x then x*2 else x)

f11c :: Integral a => [a] -> [a]
f11c = map (\x -> if odd x then 0 else x)

f11d :: Integral a => a -> [a] -> [a]
f11d k = filter (<=k)

f11e :: Integral a => [a] -> [a]
f11e = filter (<0)

f11f :: Integral a => [a] -> [a]
f11f = filter (\x -> x<0 || odd x)
{-
 1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
     Преобразовать его следующим образом:
  a) отфильтровать список так, чтобы в нём остались точки из заданной координатной четверти;
  b) преобразовать декартовы координаты в полярные.
-}

type Point = (Double, Double)

fromQuarter :: Int -> [Point] -> [Point]
fromQuarter q = filter (\x -> defQuarter x == q)
	where defQuarter (x, y) =
		if x>=0 then (if y>=0 then 1 else 4)
		else (if y>=0 then 2 else 3)

dec2pol :: Point -> Point
dec2pol (x, y) = (sqrt (x^2 + y^2), atan (y/x))

{-
 1.3 Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
  b) Извлечь из него подсписок слов заданной длины.
  c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
-}

f13a :: [String] -> [String]
f13a = map (map (\x -> if x >= 'a' && x <= 'z' then toEnum (fromEnum x - 32) else x))

f13b :: Int -> [String] -> [String]
f13b n = filter (\x -> length x<n)

f13c :: Char -> [String] -> [String]
f13c f = filter (\x -> (not . null) x && head x == f)

{-
2. Формирование числовых последовательностей (iterate).
 a) Список натуральных чисел, начиная с 0.
 b) Список чётных чисел.
 c) Список элементов последовательности: a0=1, an=(1+an-1)/2.
 d) Список символов английского алфавита.
 e) Список строк, представляющих n-значные двоичные числа.
-}

nats :: [Integer]
nats = iterate (+1) 0

evenNums :: [Integer]
evenNums = iterate (+2) 2

f2c :: [Int]
f2c =  iterate (\x -> (1+x) `div` 2) 1

engAlph :: [Char]
engAlph = (take 26 (iterate func 'a')) ++ (take 26 (iterate func 'A'))
	where func x = toEnum (fromEnum x + 1)

f2e :: Int -> [String]
f2e n = map supp (filter (\x -> length x <= n) (map toBin (take (2^n) (iterate (+1) 0))))
	where
		toBin 0 = []
		toBin n = toBin (n `div` 2) ++ [toEnum ((n `mod` 2) + fromEnum '0')]
		supp = (\x -> (replicate (n - length x) '0') ++ x)

{-
3. Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...
  b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
     координаты точек, лежащие в одной координатной четверти.
  c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
     Последний подсписок может содержать менее n элементов.
  d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
     длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.
  e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.
-}

f3a :: [Char] -> [[Char]]
f3a = groupBy (\x y -> (isDigit x && isDigit y) || ((not . isDigit) x && (not . isDigit) y))

f3b :: [Point] -> [[Point]]
f3b = groupBy (\x y -> defQuarter x == defQuarter y)
	where defQuarter (x, y) =
		if x>=0 then (if y>=0 then 1 else 4)
		else (if y>=0 then 2 else 3)

f3c :: [a] -> Int -> [[a]]
f3c xs n
  | null xs = []
  | otherwise = f : f3c s n
      where (f, s) = splitAt n xs

f3d :: [a] -> Int -> Int -> [[a]]
f3d xs n m
  | null xs = []
  | otherwise = f : f3d s n m
    where f = take n xs
          s = drop m xs

f3e :: Eq a => [a] -> Int
f3e = maximum . (map length) . group

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]
-- True
{-
4. Разные задачи.
 a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
    всех упоминающихся в тексте чисел.
 b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
    (например: все чётные от 1 до 106).
 c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
    в строке символов.
 d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
    называется элемент, больший своих соседей.
 e) Дан список. Продублировать все его элементы.
-}

f4a :: String -> Int
f4a xs = length $ filter func $ words xs
    where func xs = length (filter isDigit xs)==length xs

f4b :: (Int -> Bool) -> Int -> Int -> Int
f4b pred a b = sum $ filter pred $ dropWhile (\x -> x<a) $ takeWhile (\x -> x<b) fibs
	where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

f4c :: Int -> String -> [Char]
f4c n s = ((map snd) . (take n) . reverse . sort) $ map func $ (group . sort) s
	where func x = (length x, head x)

f4d :: (Num a, Ord a) => [a] -> [a]
f4d xs
	| length xs < 3 = []
	| otherwise =  map getElem $ filter isMax groups
		where
			groups = filter (\x -> length x == 3) $ f3d xs 3 1
			isMax [x1, x2, x3] = x2>x1 && x2>x3
			getElem [x1, x2, x3] = x2

f4e :: [a] -> [a]
f4e = foldr (\x acc -> x : x : acc) []
