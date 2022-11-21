module Eks2016 where

append :: [a] -> [a] -> [a]
--append a b = foldr (\x -> (++) [(head a)] . reverse) a b
append li1 li2 = foldr (\x y -> x:y) li2 li1


lengthsum :: (Num a, Num b) => [a] -> (b, a)
lengthsum li = foldr (\x (l,s) -> (l+1,s+x)) (0,0) li


inList :: (Eq a) => a -> [a] -> Bool
inList num li = foldl (\x y -> if y==num then True || x else False || x) False li
