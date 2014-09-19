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

even_nums :: [Integer]
even_nums = iterate (+2) 2

eng_alph :: [Char]
eng_alph = (take 26 (iterate func 'a')) ++ (take 26 (iterate func 'A'))
	where func x = toEnum (fromEnum x + 1)

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

f3d :: [a] -> Int -> Int -> [[a]]
f3d xs n m = undefined

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

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
