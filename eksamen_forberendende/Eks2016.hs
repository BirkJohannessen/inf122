module Eks2016 where

append :: [a] -> [a] -> [a]
append a b = foldr (\x -> (++) [(head a)] . reverse) a b
