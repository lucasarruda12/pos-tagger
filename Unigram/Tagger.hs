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
populateSearchTree handle ut = catch
  (do
    tp  <- fmap (parse' taggedpos) (hGetLine handle)
    ut' <- pure $ ut `add` tp
    populateSearchTree handle ut')
  (\e -> do 
    let _ = e :: IOException
    pure ut)

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

tagWords :: UnigramTree -> [POS] -> [TaggedPOS]
tagWords ut ps = fmap (tagWord ut) ps
  where 
    tagWord :: UnigramTree -> POS -> TaggedPOS
    tagWord ut p = case Map.lookup p ut of
      Nothing   -> (CC, p)
      (Just t)  -> (t, p)

loop :: IO () -> IO ()
loop = sequence_ . repeat 

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  handle <- openFile "./Unigram/Driver.data" ReadMode 
  ut     <- populateSearchTree handle (Map.empty)
  -- putStrLn (show ut)

  loop (do 
    ws     <- fmap (splitOn ' ') getLine
    tags   <- pure $ tagWords ut ws
    putStrLn (show tags))

  hClose handle

