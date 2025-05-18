module Main where

import Common.POS
import Common.Tag
import Common.Parser
import Common.Table
import Bigram.Common
import qualified Data.Map as Map
import System.IO

type Bigrams = FrequencyTable Bigram Tag
   
main :: IO ()
main = 
  readBigramsFromStdin >>=
  pure . prune >>=
  putStrLn . prettyPrint

readBigramsFromStdin :: IO Bigrams
readBigramsFromStdin = accumulateBigrams emptyft
  where 
    accumulateBigrams :: Bigrams -> IO Bigrams
    accumulateBigrams bm = do
      eof <- isEOF
      if eof then pure bm else do
        tpos <- parse' line <$> getLine
        bm' <- pure $ addMany (bigrams . reverse $ tpos) bm
        accumulateBigrams bm'

bigrams :: [TaggedPOS] -> [(Bigram, Tag)]
bigrams [] = []
bigrams ((t, p):[]) = ((Bigram p "BOS"), t):[]
bigrams ((t, p) : given@(_, gp) : xs) = ((Bigram p gp), t) : bigrams (given:xs)

prettyPrint :: [(Bigram, Tag)] -> String
prettyPrint [] = ""
prettyPrint ((a, b):[]) = show a ++ "_" ++ show b 
prettyPrint ((a, b):xs) = show a ++ "_" ++ show b ++ "\n" ++ prettyPrint xs
