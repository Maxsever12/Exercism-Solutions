module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Empty | Connect a (BST a, BST a) deriving (Show, Eq)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Connect _ (b, _)) = Just b

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Connect _ (_, b)) = Just b

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Connect a (_,_)) = Just a

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList a = foldl (flip insert) Empty a

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Connect x (Empty, Empty)
insert x (Connect a (l, r))
  | x > a     = Connect a (l, insert x r)
  | otherwise = Connect a (insert x l, r)

singleton :: a -> BST a
singleton x = Connect x (Empty, Empty)

toList :: BST a -> [a]
toList Empty = []
toList (Connect a (l, r)) = toList l ++ [a] ++ toList r
