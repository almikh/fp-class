{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}

import Data.Char
import Data.List
import System.Environment
import qualified Data.Map as Map

showPairS :: (String, Integer) -> String
showPairS (s, c) = s ++ " -> " ++ (show c)

showPairC :: (Char, Integer) -> String
showPairC (s, c) = [s] ++ " -> " ++ (show c)

createDictionary :: (Ord a) => [a] -> Map.Map a Integer
createDictionary = foldl (\d x -> Map.insertWith (+) x 1 d) Map.empty

frequentPunktuationMark :: String -> (Char, Integer)
frequentPunktuationMark xs = Map.foldlWithKey foldFunc ('0', 0) dictionary
  where
    dictionary = createDictionary $ filter isPunctuation xs
    foldFunc acc@(k,v) key val = if val>v then (key, val) else acc

frequentWords :: String -> [(String, Integer)]
frequentWords xs = take 50 $ sortBy sortFunc $ Map.toList dictionary
  where
    dictionary = createDictionary $ words $ filter (not . isPunctuation) xs
    sortFunc (_, x) (_, y) = y `compare` x

frequencyCharacters :: String -> [(Char, Integer)]
frequencyCharacters = Map.toList . createDictionary

frequencyBigram :: String -> [(String, Integer)]
frequencyBigram = filter (\(s, _) -> length s == 2) . frequentWords

frequencyTrigram :: String -> [(String, Integer)]
frequencyTrigram = filter (\(s, _) -> length s == 3) . frequentWords

{-
  Использование:
  ===
  05-analyze-text <file> <task>
  где
    task == 1 - частота знаков пунктуации
    task == 2 - частота слов
    task == 3 - частоты символов, биграмм и триграмм
-}

main = do
  [file, task] <- getArgs
  contents <- readFile file
  if (read task == 1) then (print $ showPairC $ frequentPunktuationMark contents)
  else if (read task == 2) then (mapM_ (print . showPairS) $  frequentWords contents)
  else if (read task == 3) then (do
    putStrLn "chars: "
    putStrLn "==="
    mapM_ (print . showPairC) $  frequencyCharacters contents
    putStrLn "bigrams: "
    putStrLn "==="
    mapM_ (print . showPairS) $  frequencyBigram contents
    putStrLn "trigrams: "
    putStrLn "==="
    mapM_ (print . showPairS) $  frequencyTrigram contents)
  else if (read task == 4) then error "error..."
  else (return())
