module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

insertEdge :: (Ord n) => n -> n -> Graph n -> Graph n

--insertEdge n1 n2 graph = Map.insertWith
insertEdge n1 n2 g
    | Map.notMember n2 g = insertEdge n1 n2 (Map.insert n2 Set.empty g)
    | otherwise = Map.insertWith Set.union n1 (Set.singleton n2) g
