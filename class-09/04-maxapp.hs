import Control.Applicative

{-
  Пользуясь возможностями аппликативных функторов, определите функцию,
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp2 lhs rhs = pure (max) <*> lhs <*> rhs

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 arg1 arg2 arg3 = max3 <$> arg1 <*> arg2 <*> arg3
  where max3 a b c = max (max a b) c

{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

{- Так и не смог придумать, как запихнуть сюда maximum или нечто подобное... -}
maxApp :: (Ord a, Applicative f) => [f a] -> f a
maxApp [x] = x
maxApp (x:xs) = max <$> x <*> maxApp xs

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}

main = do
  print $ maxApp3 (Just 21) (Just 45) (Just 5) {- Maybe -}
  print $ maxApp3 (Right "aaa") (Left "bbb") (Right "bbb") {- Either String -}
  print $ maxApp [Just 1, Just 22, Just 56, Just 74, Just 3]
  print $ maxApp [(Left "aaa"), (Right "ccc"), (Right "bbb")]
  test <- maxApp2 getLine getLine {- IO -}
  print $ test

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}
