import AbstractSet
import qualified ListSet as LS
import qualified TreeSet as TS
import System.Random
import Data.List

test1 :: [Int] -> Bool
test1 xs = ((sort . toList) (fromList xs :: LS.Set Int)) ==
        ((sort . toList) (fromList xs :: TS.Set Int))

test2 :: [Int] -> Int -> Bool
test2 xs x = (exists (fromList xs :: LS.Set Int) x) ==
        (exists (fromList xs :: TS.Set Int) x)

test3 :: [Int] -> Int -> Bool -> Bool
test3 xs x flag = (exists (fromList xs :: LS.Set Int) x) == flag &&
                  (exists (fromList xs :: LS.Set Int) x) == flag

main = do
  rand <- newStdGen
  print $ "test 1: " ++ (show $ test1 [1, 2, 3, 1])
  print $ "test 2: " ++ (show $ test2 [1, 2, 3] 2)
  print $ "test 3: " ++ (show $ test3 [] 2 False)
  print $ "test 4 (with System.Random): " ++ ((show . test1 . take 100) (randomRs (1, 100) rand))
