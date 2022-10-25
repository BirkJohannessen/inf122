module Week43Exercise1 where

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)


sumNodes :: (Num a) => RoseTree [a] -> RoseTree a
sumNodes tree = fmap (sum) tree
instance Functor RoseTree where
  fmap f (Branch a []) = (Branch (f a) [])
  fmap f (Branch a li) = (Branch (f a) (map (fmap f) li))
