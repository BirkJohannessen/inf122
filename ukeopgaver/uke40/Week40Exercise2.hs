module Week40Exercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)


toList :: BinSearchTree a -> [a]
toList Empty = []
toList (Branch t1 root t2) = toList t1 ++ [root] ++ toList t2
--toList (Branch t1 root Empty) = toList t1 ++ [root]
--toList (Branch Empty root t2) = [root] ++ toList t2
--toList (Branch Empty root Empty) = [root]