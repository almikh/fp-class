import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SeqQueue as SQ
import System.Environment
import System.Random

push :: (AbstractQueue q, Num a, Eq a) => q a -> [a] -> Int -> q a
push queue (x:xs) 0 = queue
push queue (x:xs) n = push (enqueue queue x) xs (n-1)

pop :: (AbstractQueue q, Num a, Eq a) => q a -> Int -> q a
pop queue 0 = queue
pop queue n = pop (snd $ dequeue queue) (n-1)

step :: (AbstractQueue q, Num a, Eq a) => q a -> [a] -> Int -> q a
step queue xs n = pop (push queue xs n) (n-1)

testQueue :: (AbstractQueue q, Num a, Eq a) => q a -> [a] -> Int -> Int -> q a
testQueue queue xs k n
  | k<=n = testQueue (step queue xs k) (drop k xs) (k+1) n
  | otherwise = queue

toList :: (AbstractQueue q, Num a, Eq a) => q a -> [a]
toList queue
  | isEmpty queue = []
  | otherwise = [x] ++ (toList newQueue)
      where (x, newQueue) = dequeue queue


main = do
  [steps] <- getArgs
  let n = read steps :: Int
  rand <- newStdGen
  let xs = randomRs (1, 100) rand
  let fromQ = toList $ testQueue (empty :: Q.Queue Int) xs 1 n
  let fromSQ = toList $ testQueue (empty :: SQ.Queue Int) xs 1 n
  let fromFQ = toList $ testQueue (empty :: FQ.Queue Int) xs 1 n
  print $ "eq: " ++ (show (fromQ==fromSQ && fromQ==fromFQ))

  print $ "size==n: " ++ (show ((length fromQ == n) && (length fromSQ == n) && (length fromFQ == n)))
