module Week38Exercise1 where

mySplitAt :: Integer -> [a] -> ([a],[a])

mySplitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = mySplitAt' n ls
    where
        mySplitAt' :: Integer -> [a] -> ([a], [a])
        mySplitAt' _  []     = ([], [])
        mySplitAt' 1  (x:xs) = ([x], xs)
        mySplitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = mySplitAt' (m - 1) xs