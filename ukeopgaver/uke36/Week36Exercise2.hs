module Week36Exercise2 where
import Data.Maybe
import Data.List


firstPalindrome :: String -> String
firstPalindrome ord = fst $ splitAt (length ord `div` 2) ord

midPalindrome :: String -> Char
midPalindrome ord = ord!!(length ord `div` 2)



halfPalindrome :: String -> Maybe String 
halfPalindrome ord =
    if reverse(ord) == ord then Just $ firstPalindrome ord else Nothing



decomposePalindrome :: String -> Maybe (String, Maybe Char)
decomposePalindrome ord  
    | reverse(ord) == ord && length(ord) `mod` 2 == 0 = Just (firstPalindrome(ord), Nothing)
    | reverse(ord) /= ord = Nothing
    | otherwise = Just (firstPalindrome(ord), Just $ midPalindrome(ord)) 


createPalindrome :: String -> Maybe Char -> String 
createPalindrome ord char
    | isNothing char = ord++reverse (ord) 
    | otherwise = ord++[fromJust(char)]++reverse(ord)
