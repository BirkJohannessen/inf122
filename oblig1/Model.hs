module Model (TextModel
             ,createModel
             ,nextDistribution) where
import Control.Monad
import Data.List
import Control.Arrow
import Data.Maybe

import qualified Data.Map as Map
import Data.Map (Map)

import NGram

-- The type for our Markov process text model.
type TextModel = Map NGram (Map Char Weight , Weight)

-- The empty model with no n-grams.
emptyModel :: TextModel
emptyModel = Map.empty

-- Update a model with a new n-gram followed by a character.
--
increaseWeight :: NGram -> Char -> TextModel -> TextModel
increaseWeight ngram next model
  | Map.notMember ngram model = Map.insert ngram (Map.insert next 1 Map.empty, 1) model
  | otherwise = Map.insert ngram (incr ngram next model) model

incr ::  NGram -> Char -> TextModel -> (Map Char Weight, Weight) 
incr ngram next model = (Map.insertWith (+) next 1 (fst $ fromJust $ Map.lookup ngram model) , (snd $ fromJust $ Map.lookup ngram model) + 1)

-- The distribution of next n-grams after a given one.
nextDistribution :: TextModel -> NGram -> Maybe ([(NGram, Weight)],Weight)
nextDistribution model current 
  | Map.member current model = Just $ Map.toList $ fromJust $ Map.lookup current model
  | otherwise = Nothing

-- Create an n-gram model from a string.
createModel :: Integer -> String -> TextModel
createModel n input = foldl (extract) emptyModel (gramsWithNext n input)

extract :: TextModel -> (NGram, Char) -> TextModel
extract model gramnext = increaseWeight (fst gramnext) (snd gramnext) model
