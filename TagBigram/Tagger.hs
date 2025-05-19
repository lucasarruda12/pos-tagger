module Main where

import TagBigram.Common
import Common.Tag
import Common.POS
import Common.Parser
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
  given <- tag
  char '_'
  t <- tag
  pure (Bigram predicted given, t)

populateAssociationMap :: Handle -> AssociationMap -> IO AssociationMap
populateAssociationMap h am = do
  eof <- hIsEOF h
  if eof
  then pure am
  else
    hGetLine h >>=
    pure . parse' bigramP >>=
    pure . add am >>=
    populateAssociationMap h

tagBigram :: AssociationMap -> Bigram -> (Tag, POS)
tagBigram am b@(Bigram pred given) = 
  case (Map.lookup b am) of
  Just t -> (t, pred)
  Nothing -> 
    case (Map.lookup (Bigram "UNK" given) am) of
    Just t -> (t, pred)
    Nothing -> (XX, pred)

tagSentence :: AssociationMap -> [POS] -> [(Tag, POS)]
tagSentence am ws = tagWithPrevTag BOS ws
  where
    tagWithPrevTag :: Tag -> [POS] -> [(Tag, POS)]
    tagWithPrevTag _ []     = []
    tagWithPrevTag prev (w:ws) =
      let (tag, _) = tagBigram am (Bigram w prev)
      in (tag, w) : tagWithPrevTag tag ws

prettyPrint :: [(Tag, POS)] -> String
prettyPrint [] = ""
prettyPrint [(tag, pos)] = pos ++ "_" ++ show tag
prettyPrint ((tag, pos):tpos) = pos ++ "_" ++ show tag ++ " " ++ prettyPrint tpos
