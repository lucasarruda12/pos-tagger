module Main where

import Common.Parser
import Common.POS
import Common.Tag
import qualified Data.Map as Map
import Control.Exception (catch, IOException)
import System.IO

type UnigramTree = Map.Map POS Tag

add :: UnigramTree -> TaggedPOS -> UnigramTree
add ut (t, p) = Map.insert p t ut
  
populateSearchTree :: Handle -> UnigramTree -> IO UnigramTree
populateSearchTree handle ut = do
  isEof <- hIsEOF handle
  if isEof then pure ut else
    hGetLine handle >>= -- read a line (assumed to be POS_TAG)
    pure . parse' taggedpos >>= -- parse the tpos
    pure . add ut >>= -- add it to the searchTree
    populateSearchTree handle -- keep it going!

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim xs = go xs
  where
    go [] = [[]]
    go (y:ys)
      | y == delim = [] : go ys
      | otherwise =
          case go ys of
            (z:zs) -> (y : z) : zs
            []     -> [[y]]  -- Should not happen

-- Maybe Tag to deal with the possibility that ive
-- not seen the word before. when using the UNK-tagged
-- input files, should always return a Just-valued Tag.
tagSentence :: UnigramTree -> [POS] -> [(Maybe Tag, POS)]
tagSentence ut ps = fmap (tagWord ut) ps
  where 
    tagWord :: UnigramTree -> POS -> (Maybe Tag, POS)
    tagWord ut p = 
      case ((Map.lookup p ut), p) of
      Just t -> Just t
      Nothing -> Map.lookup "UNK" ut

prettyPrint :: [(Maybe Tag, POS)] -> String
prettyPrint [] = ""
prettyPrint ((mt, p):xs) = case mt of
  (Just t)  -> p ++ "_" ++ show t ++ " " ++ prettyPrint xs
  Nothing   -> p ++ "_XX " ++ prettyPrint xs -- This part should be unreachable when using the UNK-tagged input files

main :: IO ()
main = do
  handle <- openFile "./Unigram/Driver.data" ReadMode 
  ut     <- populateSearchTree handle (Map.empty)

  sequence_ $ repeat ( 
    getLine >>= -- read a line from stdin
    pure . splitOn ' ' >>= -- split at the spaces
    pure . tagSentence ut >>= -- tag the words
    putStrLn . prettyPrint) -- and put it to stdout

  hClose handle

