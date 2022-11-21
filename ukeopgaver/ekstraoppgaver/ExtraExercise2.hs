module ExtraExercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

instance Functor BinSearchTree where
  fmap _ Empty = Empty
  fmap f (Branch t1 root t2) = Branch (fmap f t1) (f root) (fmap f t2)
