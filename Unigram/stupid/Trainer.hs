module Main where

import Common.Parser
import Common.Tag
import Common.POS
import Common.Table
import Common.Loop

import System.IO
import Data.Tuple (swap)

type Unigrams = FrequencyTable POS Tag

main :: IO ()
main =
  iterateUnless isEOF readAdd emptyft >>=
  pure . prune >>=
  pure . prettyPrint >>=
  putStrLn
  where
    readAdd :: Unigrams -> IO Unigrams
    readAdd us = 
      getLine >>=
      pure . parse' line >>=
      pure . (`addMany` us) . fmap swap

prettyPrint :: [(POS, Tag)] -> String
prettyPrint []          = ""
prettyPrint ((a,b):[])  = a ++ "_" ++ show b
prettyPrint ((a, b):xs) = a ++ "_" ++ show b ++ "\n" ++ prettyPrint xs
