module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2 = Set.intersection set1 set2 == Set.empty

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle Set.empty n1 = False
hasCycle g n1 = loop g n1 [n1]

loop :: (Ord n) => Graph n -> n -> [n] -> Bool
loop g n1 li
    | Set.member (head li) (Map.lookup n1 g) == n1 = True
    | null $ filterPrev (Map.lookup n1 g) li       = False
    | otherwise                                    = False || (loop $ map () (filterPrev (Map.lookup n1 g) li))

filterPrev :: [n] -> Set n -> [n]
filterPrev prevNodes nodes = filter (\n -> Set.member n prevNodes) nodes