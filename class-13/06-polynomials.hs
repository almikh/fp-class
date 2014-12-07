{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TupleSections #-}


import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char

import System.Directory
import System.Environment

import Control.Monad.Writer
import Data.Monoid

{-
   Определите тип для многочлена с вещественными коэффициентами.
-}
type Poly = [(Float, Int)]

float :: Parser Float
float = float' <|> fromIntegral `liftM` integer
  where
    float' = collectFloat <$> integer <*> (char '.' >> (natural <|> return 0))
    formFracPart x = (fromIntegral x) * (0.1 ^ ((length . show) x))
    collectFloat int frac =
      if int < 0 then (fromIntegral int) - (formFracPart frac)
        else (fromIntegral int) + (formFracPart frac)

{-
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}

poly :: Parser [(Float, Int)]
poly = checkSepMinus <|> poly' 1
  where
    poly' sign = (:) <$> term sign <*> (checkSepPlus <|> checkSepMinus <|> return [])
    checkSepPlus = token (char '+') >> poly' 1.0
    checkSepMinus = token (char '-') >> poly' (-1.0)
    tokenX = token (char 'x')
    deg = optional 1 (token natural)
    coef = optional 1.0 (token float)
    var = tokenX >> token (char '^')
    term sign = (,) <$> (*sign) `liftM` coef <*> ((var >> deg) <|> (tokenX >> return 1) <|> return 0)

{- старашая степеньс ненулевым коэф. - первая -}
normPoly = sortBy (\x y -> (snd y) `compare` (snd x))

{- обрезает старшие степени с нулевыми коэф. -}
cutPoly p = dropWhile (\(x, _) -> x==0.0) p

{- дополняет недостающие степени -}
completePoly :: Poly -> Poly
completePoly pol = completePoly' $ normPoly pol
  where
    completePoly' [(x, 0)] = [(x, 0)]
    completePoly' [(x, n)] = (x, n) : completePoly' [(0.0, n-1)]
    completePoly' (x:y:p) =
      if snd x == snd y + 1 then x : completePoly' (y : p)
        else x : completePoly' ((0.0, snd x - 1) : y : p)

less :: Poly -> Poly -> Bool
less ((_, n):p1) ((_, m):p2) = n < m

lessEq :: Poly -> Poly -> Bool
lessEq [] pol2 = True
lessEq pol1 [] = False
lessEq ((_, n):p1) ((_, m):p2) = n <= m

up :: (Float, Int) -> Poly -> Poly
up (v, n) = map (\(a, b) -> (a*v, b+n))

{- полиномы одинаковой степени! -}
subtr :: Poly -> Poly -> Poly
subtr p1 p2 = zipWith (\(x, n) (y, m) -> (x-y, m)) (completePoly p1) (completePoly p2)

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}
divmod :: Poly -> Poly -> (Poly, Poly)
divmod p1 p2 = divmod' [] p1 p2
  where
    divmod' stored pol1@((x, n):_) pol2@((y, m):_) =
      if pol2 `lessEq` res then divmod' upd res pol2
        else (normPoly upd, normPoly res)
        where
          res = cutPoly $ subtr pol1 $ up (x / y, n-m) pol2
          upd = (x/y, n-m) : stored

divmodTest = divmod (parse poly "-3x^5 +5x^4 + 3x -1") (parse poly "-x^2 + x+ 1") ==
            ([(3.0, 3), (-2.0, 2), (1.0, 1), (-1.0, 0)], [(3.0, 1), (0.0, 0)])

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd p1 p2 =
  if p1 `less` p2 then poly_gcd p2 p1
    else let (int, res) = divmod p1 p2 in
      if isNullRes res then p2
        else poly_gcd res p2
        where
          isNullRes [] = True
          isNullRes (r:_) = snd r == 0
          
{- прмиер и решение нашел в интернете, так что результат совпадает с точностью до константы -}
poly_gcdTest = poly_gcd (normPoly $ parse poly "x^6-4x^5+2x^4+5x^3+2x^2-4x-8")
                        (normPoly $ parse poly "x^5-x^4-x^3+x^2-4x-4") ==
              [(-1008.0, 2), (1008.0, 1), (2016.0, 0)]
{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
{-
  Я не знаю, как находит общие делители нескольких полиномов
  (неужели у нескольких полиномов всегда будет общий делитель?), поэтому
  эта функция вычисляет НОД для каждой пары и выбирает из них максимальный
-}
max_poly :: Poly -> Poly -> Poly
max_poly p1 p2
  | p1 `less` p2  = p2
  | otherwise = p1

poly_gcd_list :: [Poly] -> Poly
poly_gcd_list [p1, p2] = poly_gcd p1 p2
poly_gcd_list (p:ps) = max_poly (line p ps) (poly_gcd_list ps)
  where
    line p (p1:ps) = foldl (\acc x -> max_poly acc (poly_gcd p x)) (poly_gcd p p1) ps
{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}

parse' :: Parser a -> String -> Maybe a
parse' p s = let (result, others) = head $ apply p s
  in
    if null others then Just result
      else Nothing

poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file file = do
  isExist <- doesFileExist file
  if not isExist then return $ Left "Файла не существует!"
    else do
      contents <- readFile file
      let ps = map (parse' poly) $ lines contents
      if all isJust ps then
        return $ Right $ poly_gcd_list $ map (normPoly . fromJust) ps
        else
          return $ Left "Данные имеют некорректный формат!"

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}
main = do
  args <- getArgs
  if length args /= 1 then error "Неверное количестко параметров командной строки!"
    else do
      let filename = head args
      res <- poly_gcd_file filename
      case res of
        Left err -> putStrLn err
        Right pol1 -> print $ pol1
