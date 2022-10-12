module MultiplesOf3And5 where
 
mulOf3and5 :: [Integer] -> [Integer]
mulOf3and5 [] = []
mulOf3and5 (x:xs)
 | mod x 5 == 0 || mod x 3 == 0 = x : mulOf3and5 xs
 | otherwise = mulOf3and5 xs

solution :: Integer -> Integer
solution n = if n<0 then 0 else sum(mulOf3and5([0..n-1]))


