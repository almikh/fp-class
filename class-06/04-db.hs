{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}
import System.IO
import System.Environment
import System.Directory

{- несколько вспомогательных, чтоб потом короче писать -}
parseLine :: String -> [String]
parseLine s = excGroup ++ [group]
  where
    foldFunc (dst, acc) x = if x==';' then (dst ++ [acc], "") else (dst, acc ++ [x])
    (excGroup, group) = foldl foldFunc ([], "") s

stoi :: String -> Int
stoi = read

fio2fio :: String -> String
fio2fio s = f ++ " " ++ ((head i):".") ++ ((head o):".")
  where [f, i, o] = words s

{- Получение списка из (Курс, [(Группа, Количество_студентов)]) -}
foldFunc_Count :: [(String, [(String, Int)])] -> [String] -> [(String, [(String, Int)])]
foldFunc_Count dst [_, _, course, group] = insert dst course group
  where
    insert [] insertedCourse insertedGroup = [(insertedCourse, [(insertedGroup, 1)])]
    insert ((curCourse, groups):otherData) insertedCourse insertedGroup =
      if curCourse==insertedCourse then (curCourse, insertToGroups groups insertedGroup) : otherData
      else (curCourse, groups) : (insert otherData insertedCourse insertedGroup)
    insertToGroups [] insertedGroup = [(insertedGroup, 1)]
    insertToGroups ((curGroup, students):otherGroups) insertedGroup =
      if curGroup==insertedGroup then (curGroup, students+1):otherGroups
      else (curGroup, students) : (insertToGroups otherGroups insertedGroup)

{- Преобразование этого списка в удобочитаемый вид -}
showFunc_Count :: [(String, [(String, Int)])] -> [String]
showFunc_Count = foldl showCourse []
  where
    showCourse dst (course, groups) = ["course: " ++ course] ++ (map showGroup groups) ++ dst
    showGroup (group, students) = "  group: " ++ group ++ " -> " ++ (show students) ++ " students"

{- Получение списка элементов вида (<КУРС>_<ГРУППА>, [<студенты>]) -}
foldFunc_ToFiles :: [(String, [String])] -> [String] -> [(String, [String])]
foldFunc_ToFiles dst [name, _, course, group] = insert dst (course ++ "_" ++ group, fio2fio name)
  where
    insert [] (file, name) = [(file, [name])]
    insert ((curFile, names):files) (file, name) =
      if curFile==file then (curFile, name:names) : files
      else (curFile, names) : (insert files (file, name))


{- основные -}
parse "--mean-age" [course, group, file] = do
  contents <- readFile file
  putStrLn $ show $ foldl foldFunc 0 $ map parseLine $ lines contents
    where
      foldFunc acc [name, age, cr, gr] = if cr==course && gr==group then acc + (stoi age) else acc

parse "--count" [file] = do
  contents <- readFile file
  putStrLn $ unlines $ showFunc_Count $ foldl foldFunc_Count [] $ map parseLine $ lines contents

parse "--to-files" [file] = do
  contents <- readFile file
  mapM_ mapFunc $ foldl foldFunc_ToFiles [] $ map parseLine $ lines contents
    where mapFunc (file, content) = writeFile file $ unlines content

parse _ _ = undefined

help = [
  "Program options:",
  "",
  " --mean-age <course> <group> <file>       1st",
  " --count <file>                           2nd",
  " --to-files <file>                        3rd"]

main = do
  cmd:params <- getArgs
  if cmd=="-h" then putStrLn $ unlines help
  else parse cmd params
