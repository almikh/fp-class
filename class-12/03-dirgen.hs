{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Environment
import System.Directory
import System.FilePath
import System.Random

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

data Constraints = Constr {
      dirs :: (Int, Int),
      files :: (Int, Int),
      fileSize :: (Int, Int), {- в символах/байтах -}
      depth :: Int
    } deriving (Show)

data AppState = AppState {
      stCurDepth :: Int,
      stCurPath :: FilePath
    } deriving (Show)


genString :: Int -> ReaderT Constraints (StateT AppState IO) String
genString len = do
  gen <- liftIO $ newStdGen
  return $ take len $ randomRs ('a', 'z') gen

genNum :: (Int, Int) -> ReaderT Constraints (StateT AppState IO) Int
genNum (l, u) = do
  gen <- liftIO $ newStdGen
  return $ fst $ randomR (l, u) gen

defaultConstraints :: Constraints
defaultConstraints = Constr (1, 2) (1, 1) (1, 10) 1

{- эту функцию лучше не читать... -}
setConstraints :: [String] -> Constraints
setConstraints args = setConstraints' defaultConstraints args
  where
    setConstraints' constraints [] = constraints
    setConstraints' (Constr dr f fs dp) ("--depth" : val : others) =
      setConstraints' (Constr dr f fs (read val)) others
    setConstraints' (Constr dr f fs dp) ("--files-range" : l : u : others) =
      setConstraints' (Constr dr (read l, read u) fs dp) others
    setConstraints' (Constr dr f fs dp) ("--dirs-range" : l : u : others) =
      setConstraints' (Constr (read l, read u) f fs dp) others
    setConstraints' (Constr dr f fs dp) ("--file-size" : l : u : others) =
      setConstraints' (Constr dr f (read l, read u) dp) others

genDir :: FilePath -> ReaderT Constraints (StateT AppState IO) FilePath
genDir path = do
  name <- genString 6
  let newDir = path </> name
  exists <- liftIO $ doesFileExist newDir
  if exists then
    genDir path
    else return (newDir)

fillDirs :: FilePath -> ReaderT Constraints (StateT AppState IO) ()
fillDirs path = do
  range <- files `liftM` ask
  size <- fileSize `liftM` ask
  filesNumber <- genNum range
  forM_ [1 .. filesNumber] $ \_ -> do
    name <- genString 6
    curSize <- genNum size
    content <- genString curSize
    let filePath = path </> name
    exists <- liftIO $ doesFileExist filePath
    if exists then fillDirs path
      else liftIO $ writeFile filePath content


generateDirs :: ReaderT Constraints (StateT AppState IO) ()
generateDirs = do
  st <- lift $ get
  let path = stCurPath st
  liftIO $ createDirectory path
  fillDirs path

  maxDepth <- depth `liftM` ask
  dirsRange <- dirs `liftM` ask

  let curDepth = stCurDepth st
  when (curDepth < maxDepth) $ do
    dirsNumber <- genNum dirsRange

    forM_ [1 .. dirsNumber] $ \_ -> do
      newDir <- genDir path
      lift $ put $ st {stCurDepth = curDepth+1, stCurPath = newDir}
      generateDirs

runGenerator :: [String] -> FilePath -> IO ()
runGenerator args root = evalStateT (runReaderT generateDirs config) state
    where
      config = setConstraints args
      state = AppState 0 root

{-
Пример (Win):
03-dirgen temp/ --depth 3 --files-range 1 5 --dirs-range 2 4 --file-size 128 256
-}

main = do
  (root:args) <- getArgs
  runGenerator args root
  when (null args) $ do
    putStrLn "Использование: 03-dirgen [root-dir] [options]"
    putStrLn ""
    putStrLn "Опции:"
    putStrLn "  --depth [depth]            Глубина дерева (default 1)"
    putStrLn "  --files-range [low, up]    Диапазон кол-ва файлов в директории (default (1, 1))"
    putStrLn "  --dirs-range [low, up]     Диапазон кол-ва поддиректорий (default (1, 2))"
    putStrLn "  --file-size [min, max]     Размеры файлов (default (1, 10))"
