module Fold where
import Data.Sequence (Seq(), zip)
import Data.Bitraversable (Bitraversable)


sum' :: [Integer] -> Integer
sum' li = foldr (+) 0 li


factorial' :: Integer -> Integer
factorial' fact = foldr (*) 1 [1..fact] 

head' :: [a] -> Maybe a
head' li = foldr (\a b -> Just a) Nothing li

--foldl
-- foldl akumilerer utgansverdien i løpet av rekursjonen
-- ikke produktiv

sum'' :: [Integer] -> Integer
sum'' li = foldl (+) 0 li


data BinTree a = Empty
            | Branch (BinTree a) a (BinTree a)
    deriving (Eq, Show)

elim :: b -> (b -> a -> b -> b) -> BinTree a -> b

elim z f Empty = z 
elim z f (Branch leftSubTree a rightSubTree) = f (elim z f leftSubTree) a (elim z f rightSubTree)

height :: BinTree a -> Integer

height = elim 0 (\l _ r -> 1 + max l r)
--height Empty = 0
--height (Branch l _ r) = max (height l) (height r)

-- map f = foldr (\a -> (f a):) []
-- map f = foldr ((:).f) []
-- map   = flip foldr [] . ((:).)