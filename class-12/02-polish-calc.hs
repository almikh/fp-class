{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import Data.Maybe
import Data.Monoid
import Control.Applicative
import System.Environment

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader

type Stack = [Int]
type Vars = [(String, Int)]

push :: Int -> MaybeT (ReaderT Vars (WriterT (Sum Int) (State Stack))) ()
push x = lift $ lift $ tell (Sum 1) >> get >>= put . (x:)

pop :: MaybeT (ReaderT Vars (WriterT (Sum Int) (State Stack))) Int
pop = do
  lift $ lift $ tell (Sum 1)
  xs <- get
  guard $ not. null $ xs
  put $ tail xs
  return $ head xs

read' :: String -> MaybeT (ReaderT Vars (WriterT (Sum Int) (State Stack))) Int
read' s = do
  val <- lift $ asks (lookup s)
  return $ fromJust val
  if isJust val then
    return $ fromJust val
    else return $ read s


evalRPN :: String -> Vars -> (Maybe Int, Int)
evalRPN xs vars =
  let ((value, ops), stack) = runState (runWriterT (runReaderT (runMaybeT $ (mapM step $ words xs)) vars)) []
  in case value of
    Just _ -> (Just $ head stack, getSum ops)
    Nothing -> (Nothing, getSum ops)
    where
      step "+" = processTops (+)
      step "*" = processTops (*)
      step  n  = read' n >>= push
      processTops op = do
        cur <- op `liftM` pop `ap` pop
        push cur

{- Принимает строки в формате переменная=значение -}
parseVarsList :: [String] -> Vars
parseVarsList = map split
  where
    split str = (takeWhile (/='=') str, read $ tail $ dropWhile (/='=') str)

tests = [("x 3 + 4 *", ["x=3"]), ("+ * 2 4", []), ("* * *", []), ("* 1", []), ("x y +", ["x=4", "y=5"])]

main = do
  {-
  args <- getArgs
  mapM_ (\(task, _) -> print $ task ++ " -> " ++ show (evalRPN task $ parseVarsList args)) tests
  -}
  mapM_ (\(task, args) -> print $ task ++ " -> " ++ show (evalRPN task $ parseVarsList args)) tests
