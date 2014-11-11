import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import System.Environment

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

getKind :: ArmorItem -> ArmorKind
getKind (ArmorItem k t) = k

getType :: ArmorItem -> ArmorType
getType (ArmorItem k t) = t

toType :: String -> ArmorType
toType "Shield" = Shield
toType "Helmet" = Helmet
toType "Gauntlets" = Gauntlets
toType "Boots" = Boots
toType "Cuirass" = Cuirass

toKind :: String -> ArmorKind
toKind "Chitin" = Chitin
toKind "Hide" = Hide
toKind "Leather" = Leather
toKind "Elven" = Elven
toKind "Scaled" = Scaled
toKind "Glass" = Glass
toKind "ImperialLight" = ImperialLight

getItem :: String -> ArmorItem
getItem s = ArmorItem (toKind k) (toType t)
  where [k, t] = words s

loadInventory :: FilePath -> IO [ArmorItem]
loadInventory file = map getItem `fmap` lines `fmap` readFile file

{-
  При использовании такой функции при решении главной задачи выбирается
  только один комплект каждого вида, в противном случае функцию нужно было
  бы поменять на менее красивую, в которой Maybe был не нужен, и делать я этого не стал.
  Чисто из эстетических соображений...
-}
buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind items = if (length groups == 5) then makeKit kind groups else Nothing
  where
    groupFunc item1 item2 = getType item1 == getType item2
    groups = groupBy groupFunc $ filter (\item -> getKind item == kind) items
    makeKit k ts = Just $ ArmorKit k (map (getType . head) ts)

splitByKind :: [ArmorItem] -> [(ArmorKind, [ArmorItem])]
splitByKind items = filter (not . null . snd) $ map mapFunc [Chitin .. ImperialLight]
  where
    mapFunc k = (k, filter (\item -> getKind item == k) items)

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits its = sequence $ filter (isJust) $ map (\(kind, items) -> buildArmorKit kind items) (splitByKind its)

main = (head `liftM` getArgs) >>= loadInventory >>= print `liftM` buildKits
