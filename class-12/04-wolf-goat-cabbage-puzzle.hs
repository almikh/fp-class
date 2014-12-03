{-
  Напишите программу, решающую следующую задачу методом полного перебора:

     «Крестьянину нужно перевезти через реку волка, козу и капусту. Но лодка такова,
     что в ней может поместиться только крестьянин,  а с ним или один волк, или одна
     коза, или одна капуста.  Но если оставить волка с козой,  то волк съест козу, а
     если оставить  козу с капустой,  то коза  съест капусту.  Как перевёз свой груз
     крестьянин?»

  В качестве идеи для реализации используйте решение задачи о калотанской семье
  (kalotans-puzzle.hs).
-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Writer

import Data.List
import Data.Maybe

type Side = String
type Entity = String

data ProblemState = PS {
    depth :: Int,
    boat :: Side,
    leftSide :: [Entity],
    rightSide :: [Entity],
    actions :: [Entity],
    cargo :: Entity
  }

type NDS a = WriterT [[Entity]] (State ProblemState) a

addNothing :: Side -> [Entity] -> [Entity]
addNothing _ xs
  | length xs < 3 = "nothing" : xs
  | otherwise = xs

choiseCargo :: Side -> NDS ()
choiseCargo "left" = do
  ps <- get
  let curDepth = depth ps
  let acts = actions ps
  when (curDepth < 10) $ do
      let right = rightSide ps
      let left = leftSide ps
      let cargo' = cargo ps
      when (someoneSomeoneAte "left" left right cargo') $
        forM_ (delete (cargo ps) (addNothing "left" left)) $ \entity ->
          if entity=="nothing" then do
            put $ PS (curDepth+1) "right" left right (acts ++ ["nothing"]) "nothing"
            checkState
            else do
              put $ PS (curDepth+1) "right" (delete entity left) (entity:right) (acts ++ [entity]) entity
              checkState

choiseCargo "right" = do
  ps <- get
  let curDepth = depth ps
  when (curDepth < 10) $ do
    let right = rightSide ps
    let left = leftSide ps
    let acts = actions ps
    let cargo' = cargo ps
    when (someoneSomeoneAte "right" left right cargo') $
      forM_ (delete (cargo ps) (addNothing "right" right)) $ \entity ->
        if entity=="nothing" then do
          put $ PS (curDepth+1) "left" left right (acts ++ ["nothing"]) "nothing"
          checkState
          else do
            put $ PS (curDepth+1) "left" (entity:left) (delete entity right) (acts ++ [entity]) entity
            checkState


someoneSomeoneAte' :: [Entity] -> Bool
someoneSomeoneAte' xs
  | length xs /= 2 = False
  | otherwise = (existWolf && existGoat) || (existGoat && existCabbage)
    where
      existWolf = isJust $ find (=="wolf") xs
      existGoat = isJust $ find (=="goat") xs
      existCabbage = isJust $ find (=="cabbage") xs

someoneSomeoneAte :: Side -> [Entity] -> [Entity] -> Entity -> Bool
someoneSomeoneAte "left" left right cargo' =
  not (someoneSomeoneAte' (delete cargo' left)) && not (someoneSomeoneAte' right)
someoneSomeoneAte "right" left right cargo' =
  not (someoneSomeoneAte' left) && not (someoneSomeoneAte' (delete cargo' right))

checkState :: NDS ()
checkState = do
  ps <- lift $ get
  let side = boat ps
  let left = leftSide ps
  let acts = actions ps
  if length left == 3 then
    tell [acts]
    else choiseCargo side

main :: IO ()
main = do
  let initState = PS { depth = 0, boat="right", leftSide=[], rightSide=["goat", "cabbage", "wolf"], actions=[], cargo="nothing" }
  let (log, stack) = runState (runWriterT (choiseCargo "right")) initState
  mapM_ print $ (map (\xs -> (foldl (\x y -> x ++ y ++ " -> ") ""  xs) ++ "profit!") $ snd $ log)
