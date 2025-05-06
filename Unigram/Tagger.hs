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
populateSearchTree handle ut =
  (do
    -- Assume that every line is a tagged part of speech
    tp  <- fmap (parse' taggedpos) (hGetLine handle)
    populateSearchTree handle (ut `add` tp))
  -- Stop the loop when reach EOF
  `catch` (\(_ :: IOException) -> pure ut)

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

tagSentence :: UnigramTree -> [POS] -> [(Maybe Tag, POS)]
tagSentence ut ps = fmap (tagWord ut) ps
  where 
    tagWord :: UnigramTree -> POS -> (Maybe Tag, POS)
    tagWord ut p = ((Map.lookup p ut), p)

prettyPrint :: [(Maybe Tag, POS)] -> String
prettyPrint [] = ""
prettyPrint ((mt, p):xs) = case mt of
  Nothing   -> p ++ "_P " ++ prettyPrint xs
  (Just t)  -> p ++ "_" ++ show t ++ " " ++ prettyPrint xs

-- Since it is my first time writting a largeish
-- haskell program, some parts of it are very experimental.
-- This is one of them. I did this differently in other files
-- and don't know if i'll go back and change every time
-- i find a better way to do it
loop :: IO () -> IO ()
loop = sequence_ . repeat 

main :: IO ()
main = do
  handle <- openFile "./Unigram/Driver.data" ReadMode 
  ut     <- populateSearchTree handle (Map.empty)

  loop (do 
    ws     <- fmap (splitOn ' ') getLine
    tags   <- pure $ tagSentence ut ws
    putStrLn (prettyPrint tags))

  hClose handle

