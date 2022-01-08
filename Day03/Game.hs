module Game where

data Item = Sword | Bow | MagicWand deriving (Eq)
data Mob = Mummy | Skeleton Item | Witch (Maybe Item) deriving (Eq)

instance Show Item where
    show Sword = "sword"
    show Bow = "bow"
    show MagicWand = "magic wand"

instance Show Mob where
    show Mummy = "mummy"
    show (Skeleton Bow) = "doomed archer"
    show (Skeleton Sword) = "dead knight"
    show (Skeleton item) = "skeleton holding a " ++ show item
    show (Witch Nothing) = "witch"
    show (Witch (Just MagicWand)) = "sorceress"
    show (Witch (Just item)) = "witch holding a " ++ show item

createMummy :: Mob
createMummy = Mummy

createArcher :: Mob
createArcher = Skeleton Bow

createKnight :: Mob
createKnight = Skeleton Sword

createWitch :: Mob
createWitch = Witch Nothing

createSorceress :: Mob
createSorceress = Witch (Just MagicWand)

create :: String -> Maybe Mob
create "mummy" = Just createMummy
create "doomed archer" = Just createArcher
create "dead knight" = Just createKnight
create "witch" = Just createWitch
create "sorceress" = Just createSorceress
create _ = Nothing

equip :: Item -> Mob -> Maybe Mob
equip item (Skeleton i) = Just (Skeleton item)
equip item (Witch i) = Just (Witch (Just item))
equip _ _ = Nothing

class HasItem a where
    getItem :: a -> Maybe Item
    hasItem :: a -> Bool
    hasItem item = case getItem item of
        Just x -> True
        Nothing -> False

instance HasItem Mob where
    getItem (Skeleton i) = Just i
    getItem (Witch i) = i
    getItem _ = Nothing
