module Week41Exercise1 where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

graph0 = Map.fromList [(1,Set.fromList [2]),
                       (2,Set.fromList [3]),
                       (3,Set.fromList [3])]


disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2
  | Set.null (Set.intersection set1 set2) = True
  | otherwise = False

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle graph node 
  | not $ Map.member node graph = False
  | otherwise = loopCycle graph node (Set.singleton node)

loopCycle :: (Ord n) => Graph n -> n -> Set n -> Bool
loopCycle graph node visited
  | disjoint visited (fromJust $ Map.lookup node graph) = False || (foldl (||) False $ map (\x -> loopCycle graph x $ Set.union (Set.singleton x) visited) (Set.toList $ fromJust $ Map.lookup node graph))
  | otherwise = True





--orList :: [Bool] -> Bool
--orList li = foldl (||) False li
