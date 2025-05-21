module Main where

import Common.Tag
import Common.POS
import Common.Parser
import Trigram.Common

import qualified Data.Map as Map
import System.IO
import System.Environment (getArgs)

type AssociationMap = Map.Map Trigram Tag

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

add :: AssociationMap -> (Trigram, Tag) -> AssociationMap
add am (b, t) = Map.insert b t am

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

populateAssociationMap :: Handle -> AssociationMap -> IO AssociationMap
populateAssociationMap handle am = do
  eof <- hIsEOF handle
  if eof 
  then pure am else
    hGetLine handle >>=
    pure . parse' trigramP >>=
    pure . add am >>=
    populateAssociationMap handle

tagTrigram :: AssociationMap -> Trigram -> (Tag, POS)
tagTrigram am tg =
  case (Map.lookup tg am) of
  Just t  -> (t, predict tg)
  Nothing -> (XX, predict tg)

tagSentence :: AssociationMap -> [POS] -> [(Tag, POS)]
tagSentence am []         = []
tagSentence am (w:[])     = [tagTrigram am (Trigram w "BOS" "BOS")]
tagSentence am (w1:w2:[])     = tagTrigram am (Trigram w1 w2 "BOS") : tagSentence am [w2]
tagSentence am (w1:w2:w3:ws) = tagTrigram am (Trigram w1 w2 w3) : tagSentence am (w2:w3:ws)

prettyPrint :: [(Tag, POS)] -> String
prettyPrint [] = ""
prettyPrint [(tag, pos)] = pos ++ "_" ++ show tag
prettyPrint ((tag, pos):tpos) = pos ++ "_" ++ show tag ++ " " ++ prettyPrint tpos
