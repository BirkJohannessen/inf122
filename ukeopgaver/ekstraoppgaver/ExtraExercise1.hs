module ExtraExercise1 where

takeMaybe :: Integer -> [a] -> Maybe [a]

takeMaybe n li
  | n == 0 = Just []
  | n < 0 = Nothing 
  | n > (toInteger $ length li) = Nothing
  | otherwise = Just $ take (fromIntegral n) li
