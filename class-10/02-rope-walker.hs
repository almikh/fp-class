import Control.Monad

{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  + 2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  + 3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  + 5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either String Pole
updatePole p = if unbalanced p then causeOfFail p else Right p
  where
    unbalanced (l, r) = abs (l - r) >= balance
    causeOfFail (l, r)
      | l>r = Left "Imbalance right!"
      | l<r = Left "Imbalance left!"

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

banana :: Pole -> Either String Pole
banana _ = Left "Banana!"

landBoth :: Birds -> Pole -> Either String Pole
landBoth n (left, right) = updatePole (left + n, right + n)

unlandAll :: Pole -> Either String Pole
unlandAll _ = Right (0, 0)

taskFromList :: [String] -> [(Pole -> Either String Pole)]
taskFromList tasks = map (convert . words) tasks
  where
    convert ["B"] = banana
    convert ["L", n] = landLeft (read n)
    convert ["R", n] = landRight (read n)

simulate :: [String] -> Either String Pole
simulate xs = foldr (<=<) return (taskFromList xs) (0, 0)

tests = all test [1..5]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4
              >>= landLeft (-1) >>= landRight (-2)) == Left "Imbalance left!"
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "Banana!"
    test 4 = (simulate ["R 2", "L 3", "R -1", "B", "L 1"]) == Left "Banana!"
    test 5 = (simulate ["R 2", "L 2", "R 2"]) == Right (2, 4)
{-
  Загрузка команды из файла работает, но при ее использовании у меня
  не получается реализовать тесты по общему шаблону (<рез-т> == <истиный-результат>),
  поэтому этот  вариант тут не приведен - 4, 5 тесты - его альтернатива
-}
