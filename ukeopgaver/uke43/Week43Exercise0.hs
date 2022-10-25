module Week43Exercise0 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

toList :: BinSearchTree a -> [a]
toList Empty = []
toList t1 = foldr (:) [] t1

instance Foldable BinSearchTree where
  foldr _ init Empty = init
  foldr f ini (Branch t1 r t2) = let b' = f r (foldr f ini t2) in foldr f b' t1
