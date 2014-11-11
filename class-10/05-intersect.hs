{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
import qualified Data.List as DL
import Control.Monad

{- так и не смог придумать, что тут делать с монадой... -}
intersect :: Eq a => [[a]] -> [a]
intersect = foldr1 DL.intersect
