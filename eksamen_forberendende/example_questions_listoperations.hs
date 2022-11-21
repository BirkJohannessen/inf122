module Template where
import Data.Map (Map)
import qualified Data.Map as Map

isConsonant :: Char -> Bool
isConsonant letter = elem letter "bcdfghjklmnpqrstvwxz"

translate :: String -> String
translate word = concat [ if isConsonant x then [x] ++ "o" ++ [x] else [x] | x <- word]

translate' :: String -> String
translate' word = concat $ map (\x -> if isConsonant x then [x] ++ "o" ++ [x] else [x]) word

translate'' :: String -> String
translate'' word = do
  let a = concat $ map (\x -> if isConsonant x then [x] ++ "o" ++ [x] else [x]) word
  a

--differences :: [Integer] -> [Integer]
--differences li = map (\(x,y) -> abs $ x-y) []


everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:xs)
  | even $ length (x:xs) = everyOther xs
  | otherwise = x : everyOther xs


data N = A | B | C 
  deriving (Ord, Eq, Show)

type Graph label node = Map node (Map label node)


graph0 :: Graph Char N
graph0 = Map.fromList [(A,Map.fromList [('r',B)]) 
                      ,(B,Map.fromList [('o',B),('t',C)]) 
                      ,(C,Map.fromList [('e',A),('t',C)])]

--insertLabeledEdge g x y l = Map.insertWith (\_ m -> Map.insert l y m) x (Map.singleton l y) g
insertLabeledEdge :: Graph Char N -> N -> N -> Char -> Graph Char N
insertLabeledEdge gln n1 n2 l = case Map.lookup n1 gln of
                                Nothing -> Map.insert n1 (Map.fromList [(l,n2)]) gln
                                Just n  -> Map.insert n1 (Map.fromList ((Map.toList n) ++ [(l,n2)])) gln

goNext :: Graph Char N -> N -> Char -> Maybe N 
goNext g n1 l = do
  value <- Map.lookup n1 g
  next <- Map.lookup l value
  return next


goNext' :: Graph Char N -> N -> Char -> Maybe N 
goNext' g n1 l = Map.lookup n1 g >>= \x -> Map.lookup l x >>= \y -> return y


followPath :: Graph Char N -> N -> [Char] -> Maybe N
followPath g n [] = Just n
followPath g n (x:xs) = case Map.lookup n g of
                    (Nothing) -> Nothing
                    (Just g2) -> case Map.lookup x g2 of
                                 (Nothing) -> Nothing
                                 (Just n2) -> followPath g n2 xs




