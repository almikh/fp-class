{-
  Все задачи в этом задании должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться тремя различными тестовыми примерами, которые при запуске
  возвращают True, например:

  f = undefined -- решение
  f_test1 = f undefined == undefined -- тест 1
  f_test2 = f undefined == undefined -- тест 2
  f_test3 = f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции обработки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти сумму и произведение элементов списка вещественных чисел.
  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход).
  d) Найти минимальный элемент списка.
  e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию).
-}

import Data.List

f1a :: [Int] -> Int
f1a = foldl (\acc x -> if even x then acc+x else acc) 0

f1a_test1 = f1a [1, 2, 3] == 2 -- тест 1
f1a_test2 = f1a [3, 1] == 0 -- тест 2
f1a_test3 = f1a [2, 4, 6] == 12 -- тест 3


f1b :: Floating a => [a] -> (a, a)
f1b = foldl (\(s, p) x -> (s + x, p * x)) (0, 1.0)

f1b_test1 = f1b [1, 2, 3] == (6, 6) -- тест 1
f1b_test2 = f1b [3, 0] == (3, 0) -- тест 2
f1b_test3 = f1b [] == (0, 1) -- тест 3


f1c :: Floating a => [a] -> a
f1c [] = 0.0
f1c xs = (fst data_) / (snd data_)
  where data_ = foldl (\(s, l) x -> (s + x, 1 + l)) (0, 0) xs

f1c_test1 = f1c [1, 2, 3] == 2 -- тест 1
f1c_test2 = f1c [3, 0] == 1.5 -- тест 2
f1c_test3 = f1c [] == 0 -- тест 3


f1d :: (Ord a, Floating a) => [a] -> a
f1d = foldl1 (\acc x -> min acc x)

f1d_test1 = f1d [1, 2, 3] == 1 -- тест 1
f1d_test2 = f1d [3, 2, 1] == 1 -- тест 2


f1e :: [Int] -> Int -> Int
f1e xs def = foldl (\acc x -> if odd x then (if acc==def || x<acc then x else acc) else acc) def xs

f1e_test1 = f1e [1, 2, 3] 0 == 1 -- тест 1
f1e_test2 = f1e [2, 4] 0 == 0 -- тест 2
f1e_test3 = f1e [3, 1, 5] 0 == 1 -- тест 3

{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий каждый второй элемент исходного.
  b) Сформировать список, содержащий первые n элементов исходного.
  c) Сформировать список, содержащий последние n элементов исходного.
  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  e) Сформировать список, содержащий все локальные минимумы исходного списка.
  f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки.
  g) Разбить список на непересекающиеся подсписки длиной n элементов.
  h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).
  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
  l) Повторить каждый элемент списка заданное количество раз.
  m) Удалить из списка повторяющиеся подряд идущие элементы.
  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}

f2a :: Num a => [a] -> [a]
f2a = fst . foldl (\(dst, k) x -> if odd k then (dst ++ [x], k + 1) else (dst, k + 1)) ([], 0)

f2a_test1 = f2a [] == [] -- тест 1
f2a_test2 = f2a [1] == [] -- тест 2
f2a_test3 = f2a [1, 2, 3, 4, 5, 6, 7] == [2, 4, 6] -- тест 3


f2b :: Num a => [a] -> Int -> [a]
f2b xs n = take n $ foldl (\acc x -> acc ++ [x]) [] xs

f2b_test1 = f2b [] 4 == [] -- тест 1
f2b_test2 = f2b [1] 4 == [1] -- тест 2
f2b_test3 = f2b [1, 2, 3, 4, 5, 6, 7] 4 == [1, 2, 3, 4] -- тест 3


f2c :: Num a => [a] -> Int -> [a]
f2c xs n = reverse $ take n $ foldr (\x acc -> acc ++ [x]) [] xs

f2c_test1 = f2c [] 4 == [] -- тест 1
f2c_test2 = f2c [1] 4 == [1] -- тест 2
f2c_test3 = f2c [1, 2, 3, 4, 5, 6, 7] 4 == [4, 5, 6, 7] -- тест 3


f2d :: (Num a, Ord a) => [a] -> [a]
f2d xs = fst $ foldl (\(dst, neigh) x -> if x>neigh then (dst ++ [x], x) else (dst, x)) ([], head xs) xs

f2d_test1 = f2d [1, 2, 3] == [2, 3] -- тест 1
f2d_test2 = f2d [3, 2, 1] == [] -- тест 2
f2d_test3 = f2d [1, 7, 5, 4, 9, 6] == [7, 9] -- тест 3


f2e :: (Num a, Ord a) => [a] -> [a]
f2e xs = result
  where
    func (acc, pp, p) x = if p>pp && p>x then (acc ++ [p], p, x) else (acc, p, x)
    (result, _, _) = foldl func ([], head xs, head xs) xs

f2e_test1 = f2e [1, 3, 2] == [3] -- тест 1
f2e_test2 = f2e [1, 4, 2, 7, 5] == [4, 7] -- тест 2
f2e_test3 = f2e [1, 2, 3] == [] -- тест 3


f2f :: String -> [String]
f2f xs = res : ls
  where
    (ls, res) = foldr func ([], []) xs
    func x (dst, cur) = if fromEnum x == 32 then (cur : dst, []) else (dst, x : cur)

f2f_test1 = f2f "" == [""] -- тест 1
f2f_test2 = f2f "asdfg" == ["asdfg"] -- тест 2
f2f_test3 = f2f "qwerty asdfg" == ["qwerty", "asdfg"] -- тест 3


f2g :: Eq a => [a] -> Int -> [[a]]
f2g _ 0 = []
f2g xs n = ls ++ [res]
  where
    (ls, res) = foldl func ([], []) xs
    func (dst, cur) x = if length cur < n then (dst, cur ++ [x]) else (dst ++ [cur], [x])

f2g_test1 = f2g [1, 2, 3] 0 == [] -- тест 1
f2g_test2 = f2g [1, 2, 3, 4, 5] 2 == [[1, 2], [3, 4], [5]] -- тест 2
f2g_test3 = f2g [1, 2, 3, 4, 5] 3 == [[1, 2, 3], [4, 5]] -- тест 3

{- Шедевр! -}
f2h :: Eq a => [a] -> Int -> Int -> [[a]]
f2h xs n k = foldl foldFunc [] (zip xs [0, 1 ..])
    where
      foldFunc acc (x, ind) =
        if ind `mod` (n-k) == 0 then map (mapFunc x) (acc++[[]]) else map (mapFunc x) acc
      mapFunc x ys = if length ys == n then ys else ys ++ [x]

f2h_test1 = f2h [1, 2, 3, 4, 5] 3 2 == [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5], [5]] -- тест 1
f2h_test2 = f2h [1, 2, 3, 4, 5] 4 1 == [[1, 2, 3, 4], [4, 5]] -- тест 2
f2h_test3 = f2h [1, 2, 3, 4, 5] 4 2 == [[1, 2, 3, 4], [3, 4, 5], [5]] -- тест 3


f2k :: Eq a => [a] -> (a -> Bool) -> [a]
f2k xs pred = fst $ foldl func ([], True) xs
  where func (dst, flag) x = if flag && pred x then (dst++[x], flag && pred x) else (dst, False)

f2k_test1 = f2k [1, 2] (\x->False) == [] -- тест 1
f2k_test2 = f2k [1, 2, 3, 4, 5, 6, 1, 2, 3] (<4) == [1, 2, 3] -- тест 2
f2k_test3 = f2k [1, 3, 2, 7, 3] odd == [1, 3] -- тест 3


f2l :: Eq a => [a] -> Int -> [a]
f2l xs n = foldl (\acc x -> acc ++ (replicate n x)) [] xs

f2l_test1 = f2l [1, 2] 0 == [] -- тест 1
f2l_test2 = f2l [1, 2, 3] 2 == [1, 1, 2, 2, 3, 3] -- тест 2
f2l_test3 = f2l [1, 2, 3] 1 == [1, 2, 3] -- тест 3


f2m :: (Eq a, Integral a) => [a] -> [a]
f2m xs = reverse . foldl (\acc x -> if x == head acc then acc else x : acc) [head xs] $ tail xs

f2m_test1 = f2m [1, 2] == [1, 2] -- тест 1
f2m_test2 = f2m [1, 2, 2, 2, 3] == [1, 2, 3] -- тест 2
f2m_test3 = f2m [1, 1, 2, 3, 3] == [1, 2, 3] -- тест 3


f2n :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> [a] -> [b] -> [c]
f2n func xs ys = foldr (\x acc -> func (fst x) (snd x) : acc) [] $ zip xs ys

f2n_test1 = f2n (+) [1, 2] [4, 6, 8] == [5, 8] -- тест 1
f2n_test2 = f2n (*) [1, 2] [4, 6, 8] == [4, 12] -- тест 2
f2n_test3 = f2n (+) [1, 2] [] == [] -- тест 3

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать список типа [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  с) Сформировать список из первых n чисел Фибоначчи.
  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
  e) Проверить, является ли заданное целое число простым.
-}

f3a :: Int -> Int -> Int
f3a a b = foldl (+) 0 [min a b .. max a b]

f3a_test1 = f3a 1 4 == 10 -- тест 1
f3a_test2 = f3a 4 1 == 10 -- тест 2
f3a_test3 = f3a 2 2 == 2 -- тест 3


f3b :: Integer -> Integer -> Integer
f3b a b = fst $ foldl func (0, 1) [1 .. b]
  where func (af, as) x = if x>=a then (af + x*as, x*as) else (af, x*as)

f3b_test1 = f3b 1 4 == 33 -- тест 1
f3b_test2 = f3b 3 4 == 30 -- тест 2
f3b_test3 = f3b 2 2 == 2 -- тест 3


f3c :: Integer -> [Integer]
f3c n = result
  where
    func (dst, pp, p) _ = (dst ++ [pp], p, p+pp)
    (result, _, _) = foldl func ([], 1, 1) [1 .. n]

f3c_test1 = f3c 1 == [1] -- тест 1
f3c_test2 = f3c 3 == [1, 1, 2] -- тест 2
f3c_test3 = f3c 5 == [1, 1, 2, 3, 5] -- тест 3


f3d :: Floating a => a -> Integer -> a
f3d x n = fst $ foldl func (x, x) [fromInteger (2*k-1) | k <- [2..n]]
  where
    func (range, cur) k = (range - cur*x*x/(k*(k-1)), -cur*x*x/(k*(k-1)))


f3e :: Integer -> Bool
f3e x = foldl (\acc n -> if not acc || x `mod` n == 0 then False else True) True [2 .. hlimit]
  where hlimit = (floor . sqrt . fromInteger) x

f3e_test1 = f3e 2 == True -- тест 1
f3e_test2 = f3e 17 == True -- тест 2
f3e_test3 = f3e 8 == False -- тест 3

{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 3) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}

{- Путь - направление спуска -}
{- Сам с трудом понимаю, как у меня ЭТО работает =) -}
findPath :: [[Int]] -> (Int, [Int])
findPath (top : other) = maximum $ foldl downstep acc lst
  where
    acc = [(head top, [])]
    lst = (map (map (\x -> (x, []))) other)
    downstep upper lower = zipWith zipFun lower $ zipWith max' ((0, []):upper) (upper ++ [(0, [])])
    max' (a, pa) (b, pb) = if a>b then (a, pa++[1]) else (b, pb++[0])
    zipFun (val1, _) (val2, path) = (val1 + val2, path)

f4_test1 = findPath [[3],[7,4],[2,4,6],[8,5,9,3]] == (23, [0, 1, 1])
f4_test2 = findPath [[7],[3,8],[8,1,0],[2,7,4,4],[4,5,2,6,5]] == (30, [0, 0, 1, 0])
f4_test3 = findPath [[55],[94,48],[95,30,96],[77,71,26,67]] == (321, [0, 0, 0])

{-
 5. Пусть числовые матрицы представлены списками строк. Реализовать следующие функции:
  1) транспонирование матрицы;
  2) сумма двух матриц;
  3) произведение двух матриц.
-}

type Mati = [[Int]]

{- Еще 3 шедевра (хотя идею первого я подсмотрел...) -}
f51 :: Mati -> Mati
f51 = foldr (zipWith (:)) (repeat [])

f51_test1 = f51 [[1, 2], [3, 4]] == [[1, 3], [2, 4]]
f51_test2 = f51 [[1, 2], [3, 4], [5, 6]] == [[1, 3, 5], [2, 4, 6]]


f52 :: Mati -> Mati -> Mati
f52 = zipWith (zipWith (+))

f52_test1 = f52 [[1, 2], [3, 4]] [[1, 2], [3, 4]] == [[2, 4], [6, 8]]
f52_test2 = f52 [[1, 2], [3, 4], [5, 6]] [[1, 2], [3, 4], [5, 6]] == [[2, 4], [6, 8], [10, 12]]


f53 :: [[Int]] -> [[Int]] -> [[Int]]
f53 m1 m2 = foldr (\x mat -> (foldr (\y row -> (getElem x y) : row) [] (f51 m2)) : mat) [] m1
  where
    getElem row col = sum $ zipWith (*) row col

f53_test1 = f53 [[1, 2]] [[3], [4]] == [[11]]
f53_test2 = f53 [[1, 2], [3, 4]] [[1, 2]] == [[1, 2], [3, 6]]
f53_test3 = f53 [[1, 2], [3, 4]] [[7, 8], [9, 10]] == [[25, 28], [57, 64]]

{-}
 6. Реализовать левую свёртку, пользуясь правой. Проанализировать поведение собственной реализации
  на бесконечных списках и сравнить его с поведением оригинальной foldl.
-}

f6 :: (a -> b -> a) -> a -> [b] -> a
f6 f acc ls = foldr (flip f) acc (reverse ls)
