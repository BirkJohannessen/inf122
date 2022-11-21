module EASYIO where

main :: IO ()
main = do 
  putStrLn "name: "
  name <- getLine
  putStrLn ((head $ reverse $ words name) ++ ", " ++ (unwords $ reverse $ tail $ reverse $ words name))
