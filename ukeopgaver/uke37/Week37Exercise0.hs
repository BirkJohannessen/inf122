module Week37Exercise0 where

wordifyNamesAges :: (String, Integer) -> String
wordifyNamesAges nameAge = fst nameAge++" is "++show(snd nameAge)++" years old" 

filterfifty :: (String, Integer) -> Bool
filterfifty nameAge = snd nameAge <= 50

namesAndAges :: [String] -> [Integer] -> [String]
namesAndAges names ages = map wordifyNamesAges (filter (filterfifty) (zip names ages))