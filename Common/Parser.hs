module Common.Parser where

import Control.Monad
import GHC.Base hiding (some, many)
import Common.Tag
import Common.POS

type TaggedPOS = (Tag, POS)


-- Pontutation is one of: , `` . :
pontuation :: Parser Tag
pontuation = fmap (\x -> P) 
  ( string "," <|>
    string "``" <|> 
    string "." <|>
    string ":" <|>
    string "''" <|>
    string "#" <|>
    string "-LRB-" <|>
    string "-RRB-" <|>
    string "$")

-- A word is any string of characters
-- except for an underscore
word :: Parser String
word = many (sat (/= '_'))

-- A tagged part of speech is a word
-- followed by an underscore and a tag
taggedpos :: Parser TaggedPOS
taggedpos = do
  w <- word
  _ <- char '_'
  t <- tag
  pure (t, w)

-- A line is a series of tagged parts of speech
-- separated by a space
line :: Parser [TaggedPOS]
line = taggedpos `sepBy` char ' '
--
-- A tag is one of those 36 Penn Treebank
-- tags. Pontuation and tags that contain
-- a $ are singled out because of finicky
-- haskell syntax and Read derivation
tag :: Parser Tag
tag = pontuation 
  <|> fmap (const PRPs) (string "PRP$")
  <|> fmap (const WPs) (string "WP$")
  <|> (Parser reads)
  <|> fmap (const CC) (string "CC")
  <|> fmap (const CD) (string "CD")
  <|> fmap (const DT) (string "DT")
  <|> fmap (const EX) (string "EX")
  <|> fmap (const FW) (string "FW")
  <|> fmap (const IN) (string "IN")
  <|> fmap (const JJR) (string "JJR")
  <|> fmap (const JJS) (string "JJS")
  <|> fmap (const JJ) (string "JJ")
  <|> fmap (const LS) (string "LS")
  <|> fmap (const MD) (string "MD")
  <|> fmap (const NNPS) (string "NNPS")
  <|> fmap (const NNS) (string "NNS")
  <|> fmap (const NNP) (string "NNP")
  <|> fmap (const NN) (string "NN")
  <|> fmap (const PDT) (string "PDT")
  <|> fmap (const POS) (string "POS")
  <|> fmap (const PRPs) (string "PRP$")
  <|> fmap (const PRP) (string "PRP")
  <|> fmap (const RBR) (string "RBR")
  <|> fmap (const RBS) (string "RBS")
  <|> fmap (const RB) (string "RB")
  <|> fmap (const RP) (string "RP")
  <|> fmap (const  SYM) (string "SYM")
  <|> fmap (const TO) (string "TO")
  <|> fmap (const  UH) (string "UH")
  <|> fmap (const  VBD) (string "VBD")
  <|> fmap (const VBG) (string "VBG")
  <|> fmap (const VBN) (string "VBN")
  <|> fmap (const VBP) (string "VBP")
  <|> fmap (const VBZ) (string "VBZ")
  <|> fmap (const  VB) (string "VB")
  <|> fmap (const WDT) (string "WDT")
  <|> fmap (const  WPs) (string "WP$")
  <|> fmap (const WP) (string "WP")
  <|> fmap (const WRB) (string "WRB")
  <|> fmap (const P  ) (string "P")
  <|> fmap (const BOS  ) (string "BOS")
  <|> fmap (const XX ) (string "XX")

-- Primitive Parts of a general parser,
-- used as a base for my TaggedPOS Parser
-- (unashamedly stolen from 
-- https://people.cs.nott.ac.uk/pszgmh/monparsing.pdf)

newtype Parser a = Parser {
  parse :: String -> [(a, String)]
}

instance Functor Parser where
  fmap f p = Parser $ 
    \s -> [(f a, s') | (a, s') <- parse p s]

instance Applicative Parser where
  pure a    = Parser $ \cs -> [(a,cs)]
  pf <*> pa = Parser $ \s ->
    [(f a, s'') | (f, s') <- parse pf s, (a, s'') <- parse pa s']
  
instance Monad Parser where
  p >>= f = Parser $ 
    \cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs]

instance Alternative Parser where
  empty   = Parser $ \s -> [] 
  p <|> q = Parser $ \s ->
    case parse p s of
      []  -> parse q s
      res -> res

-- Parses exactly one item
item :: Parser Char
item = Parser $ 
  \s -> case s of
        ""    -> []
        (c:s) -> [(c, s)]

-- Parses c if p c, else fails
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then pure x else empty

-- Parses a single character
char :: Char -> Parser Char
char c = sat (== c)

-- Parses a string of n characters
string :: String -> Parser String
string "" = pure ""
string (c : s) = do
  x <- char c
  xs <- string s
  pure (x : xs)

-- Parses many of
many :: Parser a -> Parser [a]
many p = some p <|> pure []

-- Parses some of
some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  pure (x:xs) 

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p >>= \x -> many (sep >> p) >>= \xs -> return (x:xs)) <|> pure []

-- I dont expect things to be
-- parsed in more than one ways
-- (should probably have made parser a function
-- into Maybe, but it is now too late)
parse' :: Parser a -> String -> a
parse' p s =
  case parse p s of
    [(x, "")] -> x
    _         -> error ("failed parsing: " ++ s)
