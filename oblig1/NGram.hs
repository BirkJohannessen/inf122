module NGram (NGram,Weight
             ,grams
             ,gramsWithNext
             ,combineGrams
             ,updateGram) where
import Control.Monad
import Data.List
import Control.Arrow

import qualified Data.Map as Map
import Data.Map (Map)

-- Rename types to clarify some type signatures later
type NGram = String
type Weight = Integer

-- Produce all n-grams contained in a given string
grams :: Integer -> String -> [NGram]
grams n inp
  | fromInteger n >= length inp = inp : []
  | otherwise =  take (fromIntegral n) inp : grams n (drop 1 inp)


-- Produce all n-grams contained in a given string, paired
-- with the subsequent character
gramsWithNext :: Integer -> String -> [(NGram,Char)]
gramsWithNext n inp = zip (grams n inp) (drop (fromIntegral n) inp)

-- Recombine a list of n-grams to a string
combineGrams :: [NGram] -> String
combineGrams [] = []
combineGrams (x:[]) = x
combineGrams (x:xs) = [head x] ++ combineGrams xs

-- Update an n-gram by adding a character to the end
-- and removing the first character.
updateGram :: NGram -> Char -> NGram
updateGram g c = tail g ++ [c]
