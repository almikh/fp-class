{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

printNFA :: NFA -> String
printNFA (alph, states, begin, _, ends) =
  "(" ++ (show alph) ++ ", " ++ (show states) ++ ", "
  ++ (show begin) ++ ", func <..>, " ++ (show ends) ++ ")"

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]
    tf 2 _ = [] {- подобные строчки пришлось добавить из-за последнего примера (nfa3) -}

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect (alph, states, begin, func, ends) = all (==True) cs
  where
    isCorrectAlph = (not . null) alph
    isCorrectStates = (not . null) states
    isCorrectBegin = begin `elem` states
    isCorrectEnds = (not (null ends)) && (all (==True) $ map (`elem` states) ends)
    cs = [isCorrectAlph, isCorrectStates, isCorrectBegin, isCorrectEnds]
{- Я так и не смог толком выяснить, чт означает корректность НКА... -}

-- в дальнейшем может пригодиться функция whileM,
-- которая собирает результаты в список, пока истинно
-- заданное условие
whileM :: m Bool -> m a -> m [a]
whileM = undefined

-- Напишите функцию, определяющую, допускает ли НКА заданное слово
accept :: NFA -> String -> Bool
accept (alph, states, begin, func, ends) str = isValidString && acceptString
  where
    isValidString = all (`elem` alph) str
    acceptString = any (==True) $ map (\state -> state `elem` ends) $ foldl foldFunc [begin] str
    foldFunc acc x =
      if null acc then []
      else foldl1 (++) $ map (\state -> if (state `elem` ends) then [] else func state x) acc

-- Постройте ещё как минимум три примера НКА
nfa1 :: NFA {- строки, которые оканчиваются на ab -> *ab -}
nfa1 = (['a','b'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 'a' = [1, 2]
    tf 1 'b' = [1]
    tf 2 'a' = [2]
    tf 2 'b' = [3]
    tf 3 _ = []

nfa2 :: NFA {- строки, где на 3 с конца позиции стоит 0 -}
nfa2 =  (['0','1'], [1, 2, 3, 4], 1, tf, [4])
  where
    tf 1 '0' = [1, 2]
    tf 1 '1' = [1]
    tf 2 _ = [3]
    tf 3 _ = [4]
    tf 4 _ = []

nfa3 :: NFA {- строки, где нечетному количеству 'b' предшествует хотя бы один символ 'a' -}
nfa3 = (['a','b'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 'a' = [1, 2]
    tf 1 'b' = [1]
    tf 2 'a' = []
    tf 2 'b' = [3]
    tf 3 'a' = [3]
    tf 3 'b' = [2]

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

testStrings = ["abab", "abb", "001", "01010", "011", "0000", "0100", "aab"]

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify nfas strs = filter (not . null . snd) $ map makePair nfas
  where makePair nfa = (nfa, filter (accept nfa) strs)

printNFAs :: [(NFA, [String])] -> [(String, [String])]
printNFAs = map (\(f, s) -> (printNFA f, s))

{-

Пример (результаты):
===

nfa_ex -> ["001","011"];
nfa1 -> ["abab", "aab"];
nfa2 -> ["001","01010","011","0000"];
nfa3 -> ["abab", "aab"];

-}
