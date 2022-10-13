module Week41Exercise1 where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

graph0 = Map.fromList [(1,Set.fromList [2]),
                       (2,Set.fromList [3]),
                       (3,Set.fromList [2])]

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2 = Set.intersection set1 set2 == Set.empty

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g n1 = loop g n1 [n1]

loop :: (Ord n) => Graph n -> n -> [n] -> Bool
loop g n1 li
    | Set.member (head li) (fromJust (Map.lookup n1 g))                  = True
    | null $ filterPrev li (Set.toList (fromJust (Map.lookup n1 g)))     = False
    | otherwise  = False || loop2 g (li++[n1]) (filterPrev li (Set.toList (fromJust (Map.lookup n1 g))))

loop2 :: (Ord n) => Graph n  -> [n] -> [n] -> Bool
loop2 g li nodes = foldl (||) False (map (\n -> loop g n (li++[n])) nodes)

filterPrev :: (Ord n) => [n] -> [n] -> [n]
filterPrev prevNodes nodes = filter (\n -> not (elem n prevNodes)) nodes