{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}
import System.Random
import System.IO
import System.Environment
import System.Directory


{- parse -}
parse "--rand-gen" (points:file:[]) = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  writeFile file $ unlines $ map show $ take n $ zipWith (\x y -> (x, y)) (randSeq gen1) (randSeq gen2)
    where
      n = read points :: Int
      randSeq gen = take n $ (randomRs (-32, 32) gen :: [Int])

parse "--most-dist" (file:[]) = do
  contents <- readFile file
  putStrLn $ show $ foldl1 foldFunc $ map (\x -> read x :: (Int, Int)) $ lines contents
    where
      dist (x1, y1) (x2, y2) = ((x1-x2)^2 + (y1-y2)^2)
      foldFunc acc x = if dist (0, 0) acc > dist (0, 0) x then x else acc

parse _ _ = undefined

help = [
  "Program options:",
  "",
  " --rand-gen <npoints> <file>      random points generation",
  " --quart <file>                   number of points in quarters",
  " --most-dist <file>               point with max distance to (0, 0)"]

{- main -}
main = do
  cmd:params <- getArgs
  if cmd=="-h" then putStrLn $ unlines help
  else parse cmd params
