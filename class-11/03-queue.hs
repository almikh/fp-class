{-
  Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue.
  -}
import Control.Monad.State

type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = do
  xs <- get
  put (xs ++ [x])

dequeue :: State Queue Int
dequeue = do
  (x:xs) <- get
  put xs
  return x

testQueue :: State Queue ()
testQueue = do
  a <- dequeue
  enqueue a
  a <- dequeue
  enqueue a

runTest = execState testQueue [1, 2, 3]
