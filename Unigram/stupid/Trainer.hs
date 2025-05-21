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
  readUntilEOF emptyft >>= 
  pure . prune >>=
  pure . prettyPrint >>=
  putStrLn

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
