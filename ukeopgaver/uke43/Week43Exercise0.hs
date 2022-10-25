module Week43Exercise0 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

instance Foldable BinSearchTree where
  foldr (BinSearchTree a -> BinSearchTree a -> BinSearchTree a) -> BinSearchTree a -> BinSearchTree a = 