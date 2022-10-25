module Week43Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

type MyGraph n = Map n (Set n)

graph0 = Map.fromList [(1,Set.fromList [2]),
                       (2,Set.fromList [3]),
                       (3,Set.fromList [2])]
instance IntegerGraph MyGraph where
