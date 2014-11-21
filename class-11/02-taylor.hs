{-
  Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и сохраняя каждое слагаемое
  в журнал посредством монады Writer. В тексте программы допускается только один вызов функции tell.
-}

import Control.Monad.Writer
import Data.Monoid
import Data.List

epsilon = 0.1e-9

{- Слагаемое, конечно, сохраняется не каждый раз, но как сделать лучше - я не придумал.
   Была идея с fold*, но она провалилась из-за недостатка нужных знаний...
-}
taylor :: Double -> Double -> Double -> Writer [Double] Double
taylor initVal initN x = tell series >> return (sum series)
  where
    takeFunc (_, x) = (abs x) > epsilon
    iterFunc (n, cur) = (n+2, -cur*x*x/n/(n-1))
    series = map snd $ takeWhile takeFunc $ iterate iterFunc (initN, initVal)

sin' :: Double -> Writer [Double] Double
sin' x = taylor x 3 x

cos' :: Double -> Writer [Double] Double
cos' x = taylor 1.0 2 x
