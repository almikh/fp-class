{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}
import Data.List
import System.IO
import System.Random
import System.Environment
import Control.Monad (when)

type Cow = Int
type Bull = Int

{- компьютер загадывает слово -}
generate :: RandomGen generator => generator -> String
generate g = fst $ head $ filter filterFunc $ iterate iterFunc ("", g)
    where
      iterFunc (str, gen) = if val `elem` str then (str, newGen) else (val:str, newGen)
        where (val, newGen) = randomR ('0', '9') gen
      filterFunc (str, _) = length str >= 4

check :: String -> String -> (Bull, Cow)
check s1 s2 = (bulls, cows)
  where
    foldFunc acc (c1, c2) = if c1==c2 then acc+1 else acc
    bulls = foldl foldFunc 0 $ zipWith (\x y -> (x, y)) s1 s2
    cows = (foldl (\acc c -> if c `elem` s1 then acc+1 else acc) 0 s2) - bulls

ask :: String -> Integer -> IO ()
ask idea n = do
  putStrLn "Number>"
  ass <- getLine
  let (bulls, cows) = check idea (take 4 ass)
  when (bulls<=4) $ do
    if (bulls==4) then putStrLn ("You win! Used " ++ (show n) ++ " rounds")
      else if (null ass) then putStrLn ("Not mastered... Answer: " ++ idea)
      else do
        putStrLn ("Bulls: " ++ (show bulls) ++ ", Cows: " ++ (show cows))
        ask idea (n+1)

main = do
  g <- newStdGen
  ask (generate g) 1
