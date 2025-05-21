module Main where

import Common.Parser
import Common.Tag
import Common.POS
import Common.Table
import System.IO
import Data.Tuple (swap)

type Unigrams = FrequencyTable POS Tag

main :: IO ()
main =
  readUntilEOF emptyft >>= -- Read stdin into a map
  pure . moveToIf (const "UNK") (< 2) >>=       -- Mark uncommon words
  pure . prune >>=                -- prune only the most common Tag
  pure . prettyPrint >>=          -- show lines as pos_tag\n
  putStrLn                        -- put it to stdout

readUntilEOF :: Unigrams -> IO Unigrams
readUntilEOF ut = do
  eof <- isEOF
  if eof then pure ut else
    getLine >>= -- Read line from stdin
    pure . (parse' line) >>= -- Parse all the tpos
    pure . (`addMany` ut) . fmap swap >>= -- add them to ut
    readUntilEOF -- Do it again!

prettyPrint :: [(POS, Tag)] -> String
prettyPrint []          = ""
prettyPrint ((a,b):[])  = a ++ "_" ++ show b
prettyPrint ((a, b):xs) = a ++ "_" ++ show b ++ "\n" ++ prettyPrint xs
  
