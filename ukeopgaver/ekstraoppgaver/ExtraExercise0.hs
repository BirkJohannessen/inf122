module ExtraExercise0 where

doubleNames :: [String] -> [String]


doubleNames [] = []
doubleNames names = map (\x -> fst x ++ "-" ++ snd x) $ filter (\x -> head (fst x) /= head (snd x)) [(x,y) | x <- names, y <- names]
