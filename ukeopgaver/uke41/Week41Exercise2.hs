module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Expr a = Var a
            | Lit Integer
            | Mul (Expr a) (Expr a)
            | Add (Expr a) (Expr a)
  deriving (Eq, Show)


eval :: (Ord variable, Num value) => Expr variable -> Map variable value -> Maybe value


eval (Lit a1) _ = Just $ fromInteger a1
eval (Var a1) map = Map.lookup a1 map

eval (Mul a1 a2) map 
  | isNothing $ eval a1 map = Nothing
  | isNothing $ eval a2 map = Nothing
  | otherwise               = Just $
                                   fromJust(eval a1 map) * fromJust (eval a2 map)

eval (Add a1 a2) map
  | isNothing $ eval a1 map = Nothing
  | isNothing $ eval a2 map = Nothing
  | otherwise               = Just $
                                   fromJust(eval a1 map) + fromJust (eval a2 map)