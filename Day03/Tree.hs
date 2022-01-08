module Tree where

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    fmap = treeMap

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

addInTree :: Ord a => a -> Tree a -> Tree a
addInTree val Empty = leaf val
addInTree val (Node left a right)
    | val == a = Node left a right
    | val < a = Node (addInTree val left) a right
    | otherwise = Node left a (addInTree val right)

leaf :: Ord a => a -> Tree a
leaf val = Node Empty val Empty

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap func (Node left val right)
    = Node (treeMap func left) (func val) (treeMap func right)

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr addInTree Empty

treeToList :: Tree a -> [a]
treeToList = treeFolds [] (\ left val right -> left ++ [val] ++ right)

treeFolds :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFolds t _ Empty = t
treeFolds t func (Node left a right)
    = func (treeFolds t func left) a (treeFolds t func right)

treeSort :: Ord a => [a] -> [a]
treeSort a = treeToList $ listToTree a