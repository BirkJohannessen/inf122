import System.IO
import System.Environment
import Text.Read (readMaybe)

import Data.Maybe
import Data.List
import NGram
import Model

import GHC.Base as Base

import qualified Data.Map as Map
import System.Random

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Codec.Compression.GZip as GZip


-- Print the usage instructions for the program.
printUsage
 = putStrLn
 $  "Usage: ramble <COMMAND> <MODELFILE> [SAMPLEFILE]\n"
 ++ "where COMMAND is one of:\n"
 ++ "   create:  Create a new model from samplefile or stdin\n"
 ++ "   on <STARTPRASE> <LENGTH>:\n"
 ++ "                Generate text from model starting\n"
 ++ "                from STARTPHRASE until total length\n"
 ++ "                reaches LENGTH.\n\n"
 ++ "Examples:\n"
 ++ "    ramble create alice.mod alice.txt\n"
 ++ "    ramble on \"Alice went\" alice.mod"

-- A global n for our n-grams.
-- If you play around with it, remember that it
-- has to be the same when creating and using a model.
-- All example models are generated with gramLen = 7
gramLen :: (Num a) => a
gramLen = 7

-- Pick out an element from a weighted list by
-- going through the list until a certain treshold has been
-- reached.
pick :: [(a,Weight)] -> Weight -> a
pick (x:[]) _ = fst x
pick (x:xs) treshold
  | (snd x) <= treshold = pick xs (treshold - (snd $ x))
  | otherwise = fst x

-- Pick a random element from a weighted list with a given
--pickRandom :: [(a,Weight)] -> Weight -> IO a
--pickRandom wl total = do
  --number <- randomRIO (0,(fromIntegral (total-1))) :: IO Int
  --return $ pick wl (toInteger number)

pickRandom :: [(a,Weight)] -> Weight -> IO a
pickRandom li total = do
  n <- randomRIO (0,((fromIntegral total)-1)) :: IO Int
  return $ pick li (toInteger n)

-- Generate a fixed amount of text from a model starting from a given
-- start string
-- :
generate :: TextModel -> String -> Integer -> IO String
generate model start amount = do
  let n = (amount-(toInteger $ length start))
  if (n <= 0) then
      return $ start
  else do
      y <- generate' model (head $ reverse $ grams gramLen start) n
      return $ (reverse $ drop (gramLen-1) $ reverse $ start )++(combineGrams y)

-- Helper function which generates n-grams from a model
generate' :: TextModel -> NGram -> Integer -> IO [NGram]
generate' _ gram 0 = do
  return []
generate' model start amount = do
  x <- pickRandom (fst $ fromJust $ nextDistribution model start) (snd $ fromJust $ nextDistribution model start)
  y <- generate' model x (amount-1)
  return $ x : y
 
-- Serialize a text model and write a handle.
writeModel :: TextModel -> Handle -> IO ()
writeModel model h
 = ByteString.hPut h $ GZip.compress
                     $ UTF8.fromString
                     $ show model

-- Read a text model from a handle.
readModel :: Handle -> IO TextModel
readModel h = do
  a <- ByteString.hGetContents h
  return $ (read (UTF8.toString $ GZip.decompress a) :: TextModel)


main = do
   args <- getArgs
   case args of
     ["create",modelFile] -> do
        modelh <- openFile modelFile WriteMode
        sample <- hGetContents stdin
        let model = createModel gramLen sample
        writeModel model modelh
        hClose modelh
     ["create",modelFile,sampleFile] -> do
        modelh <- openFile modelFile WriteMode
        sampleh <- openFile sampleFile ReadMode
        sample <- hGetContents sampleh
        let model = createModel gramLen sample
        putStrLn $ "Created model with: " ++ show (Map.size model) ++ " n-grams"
        writeModel model modelh
        hClose modelh
        hClose sampleh
     ["on",startPhrase,sLength,modelFile] -> do
        modelh <- openFile modelFile ReadMode
        model <- readModel modelh
        case readMaybe sLength of
           (Just outlength)
                  ->  generate model startPhrase outlength >>= putStrLn
           Nothing -> printUsage
        hClose modelh
     _ -> printUsage
