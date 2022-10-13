module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2 = Set.intersection set1 set2 == Set.empty

--hasCycle :: (Ord n) => Graph n -> n -> Bool
--hasCycle g n1 
--  | Map.notMember n1 g = False
--  | disjoint 