module Main where

import Common.Parser
import Common.Tag
import qualified Data.Map as Map
import System.IO
import System.Environment (getArgs)

data Cell = Cell {
  actual    :: Tag,
  predicted :: Tag 
} deriving (Eq, Ord)

type ConfusionMatrix = Map.Map Cell Int

hGetParseLine :: Handle -> IO [TaggedPOS]
hGetParseLine = (parse' line <$>) . hGetLine

readConfusionMatrix :: Handle -> Handle -> IO ConfusionMatrix
readConfusionMatrix h1 h2 = loopUntilEOF h1 h2 Map.empty

loopUntilEOF :: Handle -> Handle -> ConfusionMatrix -> IO ConfusionMatrix
loopUntilEOF predictedFile actualFile confusionMatrix = do
  eofActual    <- hIsEOF actualFile
  eofPredicted <- hIsEOF predictedFile
  if (eofActual || eofPredicted)
  then pure confusionMatrix
  else do
    (predictedTags, _) <- unzip <$> hGetParseLine predictedFile
    (actualTags, _)    <- unzip <$> hGetParseLine actualFile
    confusionMatrix'   <- pure $ foldr (\w -> Map.insertWith (+) w 1) confusionMatrix $ zipWith Cell actualTags predictedTags
    loopUntilEOF predictedFile actualFile confusionMatrix'

prettyPrint :: ConfusionMatrix -> String
prettyPrint = concat . placeLineBreaks . addCommas . asList
  where 
    asList :: ConfusionMatrix -> [Int]
    asList cm = 
      [Map.findWithDefault 0 (Cell at pt) cm
      | at <- [CC .. P], pt <- [CC .. P]]

    addCommas :: [Int] -> [String]
    addCommas []      = []
    addCommas (x:[])  = [show x]
    addCommas (x:xs)  = [show x ++ ","] ++ addCommas xs
 
    placeLineBreaks :: [String] -> [String]
    placeLineBreaks [] = []
    placeLineBreaks xs = (take 37 xs) ++ ["\n"] ++ (placeLineBreaks (drop 37 xs))

main :: IO ()
main = do
  (actualFilename : _) <- getArgs
  actualFile <- openFile actualFilename ReadMode
  
  cm <- readConfusionMatrix stdin actualFile
  putStrLn $ prettyPrint cm
  
  hClose actualFile
