module Main where

import Common.Parser
import Common.Tag
import qualified Data.Map as Map
import System.IO

type ConfusionMatrix = Map.Map (Tag, Tag) Int

hGetParseLine :: Handle -> IO [TaggedPOS]
hGetParseLine = fmap (parse' line) . hGetLine

loopUntilEOF :: Handle -> Handle -> ConfusionMatrix -> IO ConfusionMatrix
loopUntilEOF th rh cm = do
  eoft <- hIsEOF th
  eofr <- hIsEOF rh 
  if (eoft || eofr)
    then pure cm
    else do
      predictedTags <- fmap (fmap fst) $ hGetParseLine th
      realTags      <- fmap (fmap fst) $ hGetParseLine rh
      -- This is huge because i'm very proud of it.
      -- If you can, please take 5 minutos to see
      -- what i'm trying to do.
      cm' <- pure $ foldr (\w -> Map.insertWith (+) w 1) cm $ zip predictedTags realTags
      loopUntilEOF th rh cm'

prettyPrint :: ConfusionMatrix -> [[Int]]
prettyPrint = chunksOf 37 . asListInt
  where 
    asListInt :: ConfusionMatrix -> [Int]
    asListInt cm =
      [ Map.findWithDefault 0 (predicted, real) cm 
        | predicted <- [CC .. P], real <- [CC .. P] ]

    chunksOf :: Int -> [Int] -> [[Int]]
    chunksOf _ [] = []
    chunksOf x xs = let (line, rest) = splitAt x xs
                      in line : chunksOf x rest
  
main :: IO ()
main = do
  testHandle <- openFile "./Test.data" ReadMode 
  realHandle <- openFile "./penn-treebank/devset.data" ReadMode

  cm <- loopUntilEOF testHandle realHandle Map.empty
  putStrLn (show $ prettyPrint cm)

  hClose testHandle
  hClose realHandle
