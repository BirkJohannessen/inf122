{-# LANGUAGE FlexibleInstances #-}
module Week43Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

type MyGraph = Map Integer (Set Integer)

graph :: (IntegerGraph g) => g

graph = insertEdge 8 5 $ insertEdge 5 8 $ insertEdge 5 1 $ insertEdge 1 8 $ insertEdge 1 6 $ insertNode 1 $ insertNode 6 $ insertNode 3 $ insertNode 8 $ insertNode 5 emptyGraph

instance IntegerGraph MyGraph where
  emptyGraph = Map.empty
  insertNode inp graph = Map.insert inp Set.empty graph
  insertEdge n1 n2 g
    | Map.notMember n1 g = insertEdge n1 n2 (Map.insert n1 Set.empty g)
    | Map.notMember n2 g = insertEdge n1 n2 (Map.insert n2 Set.empty g)
    | otherwise = Map.insertWith Set.union n1 (Set.singleton n2) g
  nodeInGraph node graph = Map.member node graph
  edgeInGraph node1 node2 graph
    | Map.member node1 graph = Set.isSubsetOf (Set.singleton node2) (fromJust $ Map.lookup node1 graph)
    | otherwise = False
