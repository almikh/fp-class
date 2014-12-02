{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}
import System.Environment
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data.Char

type Constraints = (Int, Bool, Bool, Bool)

nullConstraints :: Constraints
nullConstraints = (1, False, False, False)

setConstraints :: [String] -> Constraints
setConstraints = setConstraints' nullConstraints
  where
    setConstraints' constr [] = constr
    setConstraints' (ml, ha, hn, hp) (x:xs)
      | x == "--min-length" = setConstraints' ((read . head) xs, ha, hn, hp) (tail xs)
      | x == "--has-alpha" = setConstraints' (ml, True, hn, hp) xs
      | x == "--has-nums" = setConstraints' (ml, ha, True, hp) xs
      | x == "--has-punkt" = setConstraints' (ml, ha, hn, True) xs

isValid :: String -> Constraints -> Bool
isValid s (minLength, hasAlpha, hasNumbers, hasPunktuation) =
  checkLength && checkAlha && checkNumbers && checkPunkt
    where
      checkLength = length s >= minLength
      checkAlha = hasAlpha==False || (any isAlpha s)
      checkNumbers = hasNumbers==False || (any isNumber s)
      checkPunkt = hasPunktuation==False || (any isPunctuation s)

getValidPassword :: MaybeT (WriterT [String] (ReaderT Constraints IO)) String
getValidPassword = do
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  {- Это самый приличный вариант использования Reader'а для isValid, до которого я додумался,
    и который мне удалось реализовать -}
  valid <- lift $ lift $ asks (isValid s)
  {- Я решил, что сохранять правильный пароль в лог не стоит... -}
  when (not valid) $ lift $ tell $ [s]
  guard (valid)
  return s

askPassword :: MaybeT (WriterT [String] (ReaderT Constraints IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Сохранение в базе данных..."


main = do
  args <- getArgs
  when ((not . null) args) $ do
    t <- runReaderT (execWriterT (runMaybeT askPassword)) (setConstraints args)
    putStr "Попытки: "
    print t
  when (null args) $ do
    putStrLn "Использование: 01-password [options]"
    putStrLn ""
    putStrLn "Опции:"
    putStrLn "  --min-length [length]   Минимальная длина пароля"
    putStrLn "  --has-alpha             В пароле должны быть буквы"
    putStrLn "  --has-nums              В пароле должны быть числа"
    putStrLn "  --has-punkt             В пароле должны быть знаки пунктуации"
