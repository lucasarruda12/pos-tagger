module Main where

import Common.Tag
import Common.POS
import Common.Parser
import Bigram.Common

import qualified Data.Map as Map
import System.IO
import System.Environment (getArgs)

type AssociationMap = Map.Map Bigram Tag

main :: IO ()
main = do
  driverFile <- fmap head getArgs
  handle <- openFile driverFile ReadMode
  am <- populateAssociationMap handle Map.empty

  sequence_ $ repeat (
    getLine >>=
    pure . words >>=
    pure . tagSentence am . reverse >>=
    putStrLn . prettyPrint . reverse)

  hClose handle

add :: AssociationMap -> (Bigram, Tag) -> AssociationMap
add am (b, t) = Map.insert b t am

bigramP :: Parser (Bigram, Tag)
bigramP = do
  predicted <- word
  char '_'
  given <- word
  char '_'
  tag <- tag
  pure (Bigram predicted given, tag)

populateAssociationMap :: Handle -> AssociationMap -> IO AssociationMap
populateAssociationMap handle am = do
  eof <- hIsEOF handle
  if eof 
  then pure am else
    hGetLine handle >>=
    pure . parse' bigramP >>=
    pure . add am >>=
    populateAssociationMap handle

tagBigram :: AssociationMap -> Bigram -> (Tag, POS)
tagBigram am b@(Bigram pred given) =
  case (Map.lookup b am) of
  Just t  -> (t, pred)
  Nothing -> 
    case (Map.lookup (Bigram pred "UNK") am) of
      Just t -> (t, pred)
      Nothing -> 
        case (Map.lookup (Bigram "UNK" "UNK") am) of
          Just t -> (t, pred)
          Nothing -> (XX, pred)

tagSentence :: AssociationMap -> [POS] -> [(Tag, POS)]
tagSentence am []         = []
tagSentence am (w:[])     = [tagBigram am (Bigram w "BOS")]
tagSentence am (w1:w2:ws) = tagBigram am (Bigram w1 w2) : tagSentence am (w2:ws)

prettyPrint :: [(Tag, POS)] -> String
prettyPrint [] = ""
prettyPrint [(tag, pos)] = pos ++ "_" ++ show tag
prettyPrint ((tag, pos):tpos) = pos ++ "_" ++ show tag ++ " " ++ prettyPrint tpos
