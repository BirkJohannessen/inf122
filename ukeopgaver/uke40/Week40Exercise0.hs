module Week40Exercise0 where

data Palindrome = EvenPalindrome String | OddPalindrome String Char deriving (Show)

palindrome :: String -> Maybe Palindrome
palindrome input
    | reverse input /= input = Nothing
    | even (length input)    = Just $ EvenPalindrome (halfPalindrome input)
    | otherwise              = Just $ OddPalindrome (halfPalindrome input) (input !! div (length input) 2)

halfPalindrome :: String -> String
halfPalindrome input = take (div (length input) 2) input


toString :: Palindrome -> String
toString (EvenPalindrome p) = p++reverse p
toString (OddPalindrome p c) = p++[c]++reverse p
