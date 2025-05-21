module Main where

import Common.Tag
import Common.POS
import Common.Parser
import Bigram.Common
import Trigram.Common
import Backoff.Common

import System.IO
import System.Environment (getArgs)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

type TriAssoc = Assoc Trigram Tag
type BiAssoc = Assoc Bigram Tag
type UniAssoc = Assoc POS Tag

main :: IO ()
main = do
  args <- getArgs
  let (unigramH:bigramH:trigramH:_) = args
  
  unigramH <- openFile unigramH ReadMode
  bigramH <- openFile bigramH ReadMode
  trigramH <- openFile trigramH ReadMode

  ba <- populate bigramH bigramP emptyAssoc
  ua <- populate unigramH unigramP emptyAssoc
  ta <- populate trigramH trigramP emptyAssoc
  
  sequence_ $ repeat (
    getLine >>=
    pure . words >>=
    pure . tagSentence ua ba ta . reverse >>=
    putStrLn . prettyPrint . reverse)

  hClose unigramH
  hClose bigramH
  hClose trigramH

unigramP :: Parser (POS, Tag)
unigramP = do
  w <- word
  char '_'
  t <- tag
  pure (w, t)

bigramP :: Parser (Bigram, Tag)
bigramP = do
  predicted <- word
  char '_'
  given <- word
  char '_'
  tag <- tag
  pure (Bigram predicted given, tag)

trigramP :: Parser (Trigram, Tag)
trigramP = do
  predicted <- word
  char '_'
  given1 <- word
  char '_'
  given2 <- word
  char '_'
  tag <- tag
  pure (Trigram predicted given1 given2, tag)

tagTrigram :: UniAssoc -> BiAssoc -> TriAssoc -> Trigram -> (Tag, POS)
tagTrigram ua ba ta t = 
  let 
    Trigram p g1 g2  = t
    tag = fromMaybe XX $
        lookupA t ta <|>
        lookupA (Bigram p g1) ba   <|>
        lookupA p ua <|>
        lookupA "UNK" ua
  in (tag, p)

tagSentence :: UniAssoc -> BiAssoc -> TriAssoc -> [POS] -> [(Tag, POS)]
tagSentence ua ba ta []         = []
tagSentence ua ba ta (w:[])     = [tagTrigram ua ba ta (Trigram w "BOS" "BOS")]
tagSentence ua ba ta (w1:w2:[])     = tagTrigram ua ba ta (Trigram w1 w2 "BOS") : tagSentence ua ba ta [w2]
tagSentence ua ba ta (w1:w2:w3:ws) = tagTrigram ua ba ta (Trigram w1 w2 w3) : tagSentence ua ba ta (w2:w3:ws)

prettyPrint :: [(Tag, POS)] -> String
prettyPrint [] = ""
prettyPrint [(tag, pos)] = pos ++ "_" ++ show tag
prettyPrint ((tag, pos):tpos) = pos ++ "_" ++ show tag ++ " " ++ prettyPrint tpos
