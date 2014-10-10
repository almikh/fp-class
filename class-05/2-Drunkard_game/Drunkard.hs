{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 54 карты.
-}

data Suit = Spades | Clubs | Diamonds | Hearts
          deriving (Show, Eq)

data Value =
          Two | Three | Four | Five |
          Six  | Seven | Eight | Nine |
          Ten | Jack | Queen | King | Ace
          deriving (Show, Eq, Ord)

data ValueEx = GrayJoker | ColorJoker
          deriving (Show, Eq, Ord)

data Card = Card { value :: Value, suits :: Suit } |
            Joker ValueEx
          deriving (Show, Eq)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ suits1) (Card _ suits2) = suits1 == suits2
sameSuit (Card _ _) (Joker _) = True
sameSuit (Joker _) (Card _ _) = True
sameSuit (Joker _) (Joker _) = True

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
beats (Card _ _) (Joker _) = GT
beats (Joker _) (Card _ _) = LT
beats (Joker _) (Joker _) = EQ
c1 `beats` c2
  | (value c1 > value c2) = GT
  | (value c1 < value c2) = LT
  | otherwise = EQ

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры):
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round cards = pair
  where
    (pair, _) = aux_game_round (cards, [])
    aux_game_round ((x:cards1, y:cards2), eqs)
      | x `beats` y == GT = ((cards1 ++ (x:y:eqs), cards2), [])
      | x `beats` y == LT = ((cards1, cards2 ++ (x:y:eqs)), [])
      | otherwise = aux_game_round ((cards1, cards2), x:y:eqs)

game_round_test1 = game_round ([Card Two Clubs, Card Seven Clubs], [Card Four Diamonds, Card Six Clubs]) ==
                              ([Card Seven Clubs], [Card Six Clubs, Card Two Clubs, Card Four Diamonds])
game_round_test2 = game_round ([Card Two Clubs, Card Four Clubs], [Card Two Diamonds, Card Six Clubs]) ==
                              ([], [Card Four Clubs, Card Six Clubs, Card Two Clubs, Card Two Diamonds])

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
    deriving Show

game :: ([Card], [Card]) -> (Winner, Int)
game cards = aux_game (cards, 0)
  where
    aux_game (([], cards2), count) = (Second, count)
    aux_game ((cards1, []), count) = (First, count)
    aux_game ((cards1, cards2), count) = aux_game $ (game_round (cards1, cards2), count+1)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}
cards1_from_pair1 = [
  Card Three Clubs, Card Seven Clubs,
  Card Four Spades, Joker ColorJoker,
  Card Two Diamonds, Card Three Clubs,
  Card King Clubs, Card Seven Hearts,
  Card Queen Spades, Card Four Diamonds]

cards2_from_pair1 = [
  Card King Spades, Card Seven Hearts,
  Card Two Diamonds, Card Jack Clubs,
  Card Six Hearts, Card Nine Clubs,
  Card Ace Clubs, Card Seven Diamonds,
  Card King Hearts, Card Eight Clubs]

cards1_from_pair2 = [
  Card Three Clubs, Card Seven Clubs,
  Card Four Spades, Joker ColorJoker,
  Card Two Diamonds, Card Three Clubs,
  Card King Clubs, Card Seven Hearts,
  Card Queen Spades, Card Four Diamonds]

cards2_from_pair2 = [
  Card King Spades, Card Seven Hearts,
  Card Two Diamonds, Card Jack Clubs,
  Card Six Hearts, Card Nine Clubs,
  Card Ace Clubs, Card Seven Diamonds,
  Card King Hearts, Card Eight Clubs]

cards1_from_pair3 = [
  Card Three Clubs, Card Seven Clubs,
  Card Four Spades, Joker ColorJoker,
  Card Two Diamonds, Card Three Clubs,
  Card King Clubs, Card Seven Hearts,
  Card Queen Spades, Card Four Diamonds]

cards2_from_pair3 = [
  Card King Spades, Card Seven Hearts,
  Card Two Diamonds, Card Jack Clubs,
  Card Six Hearts, Card Nine Clubs,
  Card Ace Clubs, Card Seven Diamonds,
  Card King Hearts, Card Eight Clubs]

cards1_from_pair4 = [
  Card Three Clubs, Card Seven Clubs,
  Card Four Spades, Joker ColorJoker,
  Card Two Diamonds, Card Three Clubs,
  Card King Clubs, Card Seven Hearts,
  Card Queen Spades, Card Four Diamonds]

cards2_from_pair4 = [
  Card King Spades, Card Seven Hearts,
  Card Two Diamonds, Card Jack Clubs,
  Card Six Hearts, Card Nine Clubs,
  Card Ace Clubs, Card Seven Diamonds,
  Card King Hearts, Card Eight Clubs]

cards1_from_pair5 = [
  Card Queen Clubs, Card King Spades,
  Card King Spades, Joker ColorJoker,
  Card Nine Diamonds, Card King Hearts,
  Card King Clubs, Joker GrayJoker,
  Card Queen Spades, Card King Diamonds]

cards2_from_pair5 = [
  Card Two Spades, Card Seven Hearts,
  Card Two Diamonds, Card Four Clubs,
  Card Six Hearts, Card Nine Clubs,
  Card Nine Clubs, Card Five Diamonds,
  Card Two Hearts, Card Five Clubs]

game_test1 = game (cards1_from_pair1, cards2_from_pair1)
game_test2 = game (cards1_from_pair2, cards2_from_pair2)
game_test3 = game (cards1_from_pair3, cards2_from_pair3)
game_test4 = game (cards1_from_pair4, cards2_from_pair4)
game_test5 = game (cards1_from_pair5, cards2_from_pair5)

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-- Некрасиво, но работает --}
type Pair = (Card, Card)

game_round_Ex :: ([Card], [Card]) -> ([Card], [Card], [Pair])
game_round_Ex cards = (c1, c2, mem)
  where
    ((c1, c2), _, mem) = aux_game_round (cards, [], [])
    aux_game_round ((x:cards1, y:cards2), eqs, mem)
      | x `beats` y == GT = ((cards1 ++ (x:y:eqs), cards2), [], (x, y):mem)
      | x `beats` y == LT = ((cards1, cards2 ++ (x:y:eqs)), [], (x, y):mem)
      | otherwise = aux_game_round ((cards1, cards2), x:y:eqs, (x, y):mem)

game_Ex :: ([Card], [Card]) -> (Winner, Int, [Pair])
game_Ex cards = aux_game (cards, 0, [])
  where
    aux_game (([], cards2), count, mem) = (Second, count, mem)
    aux_game ((cards1, []), count, mem) = (First, count, mem)
    aux_game ((cards1, cards2), count, mem) = aux_game ((c1, c2), count+1, used ++ mem)
      where (c1, c2, used) = game_round_Ex (cards1, cards2)

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
