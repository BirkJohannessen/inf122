module Main where


import Oblig0Common
  ( applyFilter,
    highPassCutoff,
    hpf,
    lowPassCutoff,
    lpf,
    zeroCrossings,
  )

import System.IO
main :: IO()
main = do
  main' [] 

-- this function only remember the last 100 inputs of summedData. this makes calculations faster, and is enough to calibrate filters correct. 
main' :: [Double] -> IO()
main' oldSummedData  = do
  input <- getLine
  let datapoints = map read (lines input) :: [(Double, Double, Double)]
  let summedData = take 100 (map (\(a, b, c) -> a + b + c) datapoints ++ oldSummedData)
  let processedData =
        applyFilter (hpf highPassCutoff) $
          applyFilter (lpf lowPassCutoff) $
            reverse summedData
  let hasStepped = zeroCrossings2 (take 2 processedData) == 1  -- only need 2 elements to check if it has crossed zero
  if hasStepped then print "Step!" else return ()
  hFlush stdout
  main' summedData

-- function that is equal to zerocrossing but only counting for half of the crossings, making it approx. equal to zerocrossings/2
zeroCrossings2 :: (Num a, Ord a) => [a] -> Integer
zeroCrossings2 [] = 0
zeroCrossings2 [x] = 0
zeroCrossings2 (x:(y:xs))
    | x < 0 && y >= 0 = 1 + zeroCrossings2 (y:xs)
    | otherwise       =     zeroCrossings2 (y:xs)