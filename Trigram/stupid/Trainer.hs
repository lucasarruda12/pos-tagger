module Main where

import Common.POS
import Common.Tag
import Common.Parser
import Common.Table
import Trigram.Common
import qualified Data.Map as Map
import System.IO

type Trigrams = FrequencyTable Trigram Tag
   
main :: IO ()
main = 
  readTrigramfromStdin >>=
  pure . prune >>=
  putStrLn . prettyPrint

readTrigramfromStdin :: IO Trigrams
readTrigramfromStdin = accumulateTrigrams emptyft
  where 
    accumulateTrigrams :: Trigrams -> IO Trigrams
    accumulateTrigrams tm = do
      eof <- isEOF
      if eof then pure tm else do
        tpos <- parse' line <$> getLine
        tm' <- pure $ addMany (trigrams . reverse $ tpos) tm
        accumulateTrigrams tm'

trigrams :: [TaggedPOS] -> [(Trigram, Tag)]
trigrams [] = []
trigrams ((t, p):[]) = ((Trigram p "BOS" "BOS"), t):[]
trigrams ((t, p):given@(_, gp):[]) = ((Trigram p gp "BOS"), t):(trigrams [given])
trigrams ((t, p) : given1@(_, gp1) : given2@(_,gp2) : xs)= ((Trigram p gp1 gp2), t) : trigrams (given1:given2:xs)

prettyPrint :: [(Trigram, Tag)] -> String
prettyPrint [] = ""
prettyPrint ((a, b):[]) = show a ++ "_" ++ show b 
prettyPrint ((a, b):xs) = show a ++ "_" ++ show b ++ "\n" ++ prettyPrint xs
