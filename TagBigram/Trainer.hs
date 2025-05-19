module Main where

import TagBigram.Common
import Common.Table
import Common.Tag
import Common.Parser
import System.IO

type Bigrams = FrequencyTable Bigram Tag

main :: IO ()
main = 
  readUntilEOF emptyft >>=
  pure . moveToIf (\(Bigram _ t) -> Bigram "UNK" t) (<2) >>=
  pure . prune >>=
  putStrLn . prettyPrint

readUntilEOF :: Bigrams -> IO Bigrams
readUntilEOF bs = do
  eof <- isEOF
  if eof
  then pure bs
  else do
    l    <- getLine
    tpos <- pure $ parse' line l
    bs' <- pure $ addMany (bigrams . reverse $ tpos) bs
    readUntilEOF bs'

bigrams :: [TaggedPOS] -> [(Bigram, Tag)]
bigrams [] = []
bigrams ((t, p):[]) = (Bigram p BOS, t):[]
bigrams ((t, p): given@(gt, _) : xs) = (Bigram p gt, t) : bigrams (given : xs)

prettyPrint :: [(Bigram, Tag)] -> String
prettyPrint [] = ""
prettyPrint ((a, b) : []) = show a ++ "_" ++ show b
prettyPrint ((a, b) : xs) = show a ++ "_" ++ show b ++ "\n" ++ prettyPrint xs

