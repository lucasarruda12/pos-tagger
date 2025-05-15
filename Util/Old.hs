module Main where

import Common.Parser
import Common.Tag
import qualified Data.Map as Map
import System.IO
import System.Environment (getArgs)

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

prettyPrint :: ConfusionMatrix -> String
prettyPrint = foldr ((++) . showtagtagint) "" . asList
  where 
    asList :: ConfusionMatrix -> [(Tag, Tag, Int)]
    asList cm = 
      [(predicted, real, Map.findWithDefault 0 (predicted, real) cm) 
        | predicted <- [CC .. P], real <- [CC .. P] ]

    showtagtagint :: (Tag, Tag, Int) -> String
    showtagtagint (t1, t2, i) = show (toNat t1) ++ " " ++ show (toNat t2) ++ " " ++ show i ++ "\n"

main :: IO ()
main = do
  args <- getArgs

  let (testFile : devSet : _) = args

  testHandle <- openFile testFile ReadMode 
  realHandle <- openFile devSet ReadMode

  cm <- loopUntilEOF testHandle realHandle Map.empty
  putStrLn (prettyPrint cm)

  hClose testHandle
  hClose realHandle
