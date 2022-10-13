module Week41Exercise1 where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2 = Set.intersection set1 set2 == Set.empty

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g n1 = loop g n1 [n1]

loop :: (Ord n) => Graph n -> n -> [n] -> Bool
loop g n1 li
    | Set.member (head li) (fromJust (Map.lookup n1 g))                  = True
    | null $ filterPrev li (Set.toList (fromJust (Map.lookup n1 g)))     = False 
    | filterPrev li (Set.toList (fromJust (Map.lookup n1 g))) == (x:xs)  = False || loop g x li++[x]


filterPrev :: [n] -> [n] -> [n]
filterPrev prevNodes nodes = filter ((elem) prevNodes) nodes