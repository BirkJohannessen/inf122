module Week42Exercise0 where
import Distribution.Simple.Utils (xargs)


applyFunctions :: [a -> b] -> [a] -> [b]
applyFunctions [] [] = []
applyFunctions (x:xs) (y:ys) = [x y] ++ applyFunctions xs ys
applyFunctions [] (y:ys) = []
applyFunctions (x:xs) [] = []