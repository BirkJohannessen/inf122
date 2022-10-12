module Week37Exercise1 where

equalCubeSum :: Integer -> [(Integer, Integer, Integer, Integer)]

equalCubeSum n = [(i,j,k,l)| i <- [1..n], j <- [1..n], k <- [1..n], l <- [1..n], (i^3+j^3)==(k^3+l^3), (i,j)/=(k,l), (i,j)/=(l,k)]