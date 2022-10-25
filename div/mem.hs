{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
module Mem where

isEven' :: Integer -> Bool
isEven' number
    | number == 1 = False
    | number == 2 = True
    | number < 0 = isEven' (-1*number)
    | otherwise = isEven' (2-number)

isPrime' ::Integral a => a -> Bool
isPrime' number = filter (\x -> (mod number x) == 0) [1..number] == [1,number]





abs' :: Integer -> Integer
abs' number = [number..(-number)] !! (fromIntegral (abs number*2))