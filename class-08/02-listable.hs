{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
  toList :: a -> [a]
  fromList :: [a] -> a

instance Listable String where
  toList = words
  fromList = unwords

instance Listable Integer where
  toList = map (\x -> (fromEnum x) - 48) . show
  fromList = read . unwords . map (show)


test1 = toList (123 :: Integer) == [1, 2, 3]
test2 = fromList ([1, 2, 3] :: [Integer]) == 123

test3 = toList "1 2 3"
test4 = fromList ["1", "2", "3"] == "1 2 3"

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integral a - любое целое число разбивается на список цифр.
-}
