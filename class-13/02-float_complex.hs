{-# LANGUAGE TupleSections #-}

import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Char

{-
  float = <число>.[<число>]
  Примеры:
    5.==5.0 ,
    5.3
    5==5.0
-}
float :: Parser Float
float = float' <|> fromIntegral `liftM` integer
  where
    float' = collectFloat <$> integer <*> (char '.' >> optional 0 natural)
    formFracPart x = (fromIntegral x) * (0.1 ^ ((length . show) x))
    collectFloat int frac =
      if int < 0 then (fromIntegral int) - (formFracPart frac)
        else (fromIntegral int) + (formFracPart frac)

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".

-}
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ (,) <$> token float <*> (token (char ',') >> token float)

complex' :: Parser (Float, Float)
complex' = complex <|> float'
  where float' = (, 0.0) <$> float

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (token complex') (symbol ";")

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
{- Не совсем понял про вспомогательные парсеры... Нижеприведенный код вроде работает. -}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ sepBy (token complex') (symbol ",")
