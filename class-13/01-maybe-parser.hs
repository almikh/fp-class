{-
   Тип Parser может быть определён следуюшим образом:
-}

import Data.Maybe
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\s -> Just (x, s))
  p >>= q = Parser $ \s ->
		let r1 = apply p s in
		case r1 of
			Just (x, s') -> apply (q x) s'
			Nothing -> Nothing

  fail _ = Parser (\_ -> Nothing)

instance MonadPlus Parser where
  mzero = Parser (\_ -> Nothing)
  p `mplus` q = Parser $ \s ->
    let r1 = apply p s in
	  if isJust r1 then r1
	    else apply q s
