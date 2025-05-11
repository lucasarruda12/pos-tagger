module Main where

import Common.Parser
import Common.Tag
import Common.POS
import qualified Data.Map as Map
import System.IO

newtype UnigramMap = UM (Map.Map POS (Map.Map Tag Int))
  deriving (Show)

-- Add the information of a single Tagged Part-of-speech
-- to the Map. create a new entry for new parts of speech
-- and add 1 to the count of that tag for the known ones.
add :: TaggedPOS -> UnigramMap -> UnigramMap
add (t, p) (UM m) = UM $ Map.insertWith (Map.unionWith (+)) p (Map.singleton t 1) m

readUntilEOF :: UnigramMap -> IO UnigramMap
readUntilEOF ut = do
  eof <- isEOF
  if eof then pure ut else
    getLine >>= -- Read line from stdin
    pure . (parse' line) >>= -- Parse all the tpos
    pure . (foldr add ut) >>= -- add them to ut
    readUntilEOF -- Do it again!

prune :: UnigramMap -> [(POS, Tag)]
prune (UM m) = Map.toList $ fmap (fst . takeLargest) m
  where 
    takeLargest :: Map.Map Tag Int -> (Tag, Int)
    takeLargest = (foldr larger (CC, 0)) . Map.toList

    larger :: (Tag, Int) -> (Tag, Int) -> (Tag, Int)
    larger t1@(_,x) t2@(_,y) 
      | x > y     = t1
      | otherwise = t2

prettyPrint :: [(POS, Tag)] -> String
prettyPrint []          = ""
prettyPrint ((a,b):[])  = a ++ "_" ++ show b
prettyPrint ((a, b):xs) = a ++ "_" ++ show b ++ "\n" ++ prettyPrint xs

main :: IO ()
main =
  readUntilEOF (UM Map.empty) >>= -- Read stdin into a map
  pure . prune >>=                -- prune only the most common Tag
  pure . prettyPrint >>=          -- show lines as pos_tag\n
  putStrLn                        -- put it to stdout
  
