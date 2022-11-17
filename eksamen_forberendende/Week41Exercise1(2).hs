module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)


disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2
  | Set.null (Set.intersection set1 set2) = True
  | otherwise = False

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle graph node 
  | = False
  | otherwise = loopCycle graph node (Set.singleton n)

loopCycle :: (Ord n) => Graph n -> n -> Set n -> Bool
loopCycle graph node visited
  | disjoint visited (Map.lookup n) = False || $ map (\x -> loopCycle graph x (Set.union (Set.singleton x) visited) (Map.lookup n)
  | otherwise = True

graphMember :: (Ord n) => Graph n -> n -> Set n -> Bool
graphMember graph n visited
  | Set.member (fst visited) 
  | otherwise = True
