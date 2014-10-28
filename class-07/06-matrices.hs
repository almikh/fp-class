{-
   Напишите программу обработки квадратных матриц (на массивах) со следующими возможностями:
   1) чтение матрицы из тестового файла;
   2) запись матрицы в текстовый файл;
   3) сумма матриц;
   4) произведение матриц.

  Задание на обработку может выглядеть, к примеру, следующим образом (здесь вычисляется матричное
  выражение (A + B) * C):

    LOAD a FROM matr1.txt
    LOAD b FROM matr2.txt
    LOAD c FROM matr3.txt
    CALC d AS PLUS a b
    CALC e AS MULT d c
    SAVE d TO matr4.txt

   Параметром командной строки должно быть имя файла со сценарием указанного или подобного ему вида.
-}

import Data.Ix
import Data.List
import System.IO
import Data.Array.IArray
import System.Environment
import qualified Data.Map as Map

type Mat = Array (Int, Int) Int

readMat :: String -> Mat
readMat contents = listArray ((1, 1), (rows, cols)) arr
  where
    temp = lines contents
    cols = length $ words $ head temp
    rows = length temp
    arr = map read $ words contents

writeMat :: Mat -> String
writeMat m = unlines $ [row2str "" i | i <- range (lr, ur)]
  where
    b@((lr, lc), (ur, uc)) = bounds m
    row2str str row = unwords $ [show (m!(row, i)) | i <- range (lc, uc) ]

sumMat :: Mat -> Mat -> Mat
sumMat x y = array b [((i, j), x!(i, j) + y!(i, j)) | i <- fRange, j <- sRange]
  where
    b@(l, u) = bounds x
    fRange = range (fst l, fst u)
    sRange = range (snd l, snd u)

prodMat :: Mat -> Mat -> Mat
prodMat x y = array newSize [((i, j), mulRowCol i j) | i <- xRowsRange, j <- yColsRange]
  where
    (xl, xu) = bounds x
    (yl, yu) = bounds y
    newSize = ((fst xl, snd yl), (fst xu, snd yu))
    xRowsRange = range (fst xl, fst xu)
    yColsRange = range (snd yl, snd yu)
    getRow i = [x!(i, j) | j <- range (snd xl, snd xu)]
    getCol j = [y!(i, j) | i <- range (fst yl, fst yu)]
    mulRowCol i j = sum $ zipWith (*) (getRow i) (getCol j)

{--------------------------}
type Storage = Map.Map String Mat

exec :: Storage -> [String] -> IO Storage
exec storage ["LOAD", name, "FROM", file] = do
  contents <- readFile file
  return $ Map.insert name (readMat contents) storage

exec storage ["WRITE", name, "TO", file] = do
  writeFile file $ writeMat $ storage Map.! name
  return $ storage

exec storage ["APPEND", name, "TO", file] = do
  appendFile file $ writeMat $ storage Map.! name
  return $ storage

exec storage [dest, "AS", "SUM", name1, name2] = do
  let
    mat1 = storage Map.! name1
    mat2 = storage Map.! name2
  return $ Map.insert dest (sumMat mat1 mat2) storage

exec storage [dest, "AS", "PROD", name1, name2] = do
  let
    mat1 = storage Map.! name1
    mat2 = storage Map.! name2
  return $ Map.insert dest (prodMat mat1 mat2) storage


execTask :: Storage -> [[String]] -> IO Storage
execTask storage [] = return (storage)
execTask storage (task:other) = do
  print $ unwords task
  newStirage <- exec storage task
  execTask newStirage other

{-
  Команды:
  ========
  LOAD <mat_name> FROM <filename>
  <mat> AS SUM <mat1> <mat2>
  <mat> AS PROD <mat1> <mat2>
  WRITE <mat1> TO <file>
  APPEND <mat1> TO <file>
-}

main = do
  [file] <- getArgs
  contents <- readFile file
  let tasks = map words $ lines $ contents
  storage <- return (Map.empty)
  execTask storage tasks
  return()
