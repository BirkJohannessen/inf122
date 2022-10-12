module Oblig0Common where


-- Compute the average of a list of values
average :: (Fractional a) => [a] -> a
average list =  sum list / fromIntegral (length list)


-- A simple lowpass filter with adjustable cut-off
lpf :: (Fractional a) => Integer -> [a] -> a
lpf n list = average $ take (fromIntegral n) list


-- A simple high pass filter with adjustable cut-off
hpf :: (Floating a) => Integer -> [a] -> a
hpf n list = head list - lpf n list

-- Extend a finite signal with an infinite constant past
extend ::  Num a => [a] -> [a]
extend [] = repeat 0
extend [x] = repeat x
extend (x:xs) = x: extend xs

-- Apply a filter to a list of values
applyFilter :: (Num a, Floating a) => ([a] -> a) -> [a] -> [a]
applyFilter fil = map fil . iterate tail . extend


-- Count the number of zero-crossings in a signal represented by a list
zeroCrossings :: (Num a, Ord a) => [a] -> Integer
zeroCrossings [] = 0
zeroCrossings [x] = 0
zeroCrossings (x:(y:xs))
    | x < 0 && y >= 0 = 1 + zeroCrossings (y:xs)
    | x > 0 && y <= 0 = 1 + zeroCrossings (y:xs)
    | otherwise       =     zeroCrossings (y:xs)

lowPassCutoff :: Integer
lowPassCutoff = 8
highPassCutoff :: Integer
highPassCutoff = 7