module Main where

import Common.Parser
import Common.POS
import Common.Tag
import qualified Data.Map as Map
import Control.Exception (catch, IOException)
import System.IO
import System.Environment (getArgs)

type UnigramMap = Map.Map POS Tag

add :: UnigramMap -> TaggedPOS -> UnigramMap
add um (t, p) = Map.insert p t um
  
populateSearchMap :: Handle -> UnigramMap -> IO UnigramMap
populateSearchMap handle um = do
  isEof <- hIsEOF handle
  if isEof then pure um else
    hGetLine handle >>= -- read a line (assumed to be POS_TAG)
    pure . parse' taggedpos >>= -- parse the tpos
    pure . add um >>= -- add it to the searchTree
    populateSearchMap handle -- keep it going!

-- Maybe Tag to deal with the possibility that ive
-- not seen the word before. when using the UNK-tagged
-- input files it should always return a Just-valued Tag.
tagSentence :: UnigramMap -> [POS] -> [(Maybe Tag, POS)]
tagSentence um ps = fmap (tagWord um) ps
  where 
    tagWord :: UnigramMap -> POS -> (Maybe Tag, POS)
    tagWord um p = 
      case (Map.lookup p um) of
      Just t -> (Just t, p)
      Nothing -> (Map.lookup "UNK" um, p)

prettyPrint :: [(Maybe Tag, POS)] -> String
prettyPrint [] = ""
prettyPrint ((mt, p):xs) = case mt of
  (Just t)  -> p ++ "_" ++ show t ++ " " ++ prettyPrint xs
  Nothing   -> p ++ "_XX " ++ prettyPrint xs -- This part should be unreachable when using the UNK-tagged input files

main :: IO ()
main = do
  driverFile <- fmap head getArgs
  handle <- openFile driverFile ReadMode 
  um     <- populateSearchMap handle (Map.empty)

  sequence_ $ repeat ( 
    getLine >>= -- read a line from stdin
    pure . words >>= -- split at the spaces
    pure . tagSentence um >>= -- tag the words
    putStrLn . prettyPrint) -- and put it to stdout

  hClose handle

