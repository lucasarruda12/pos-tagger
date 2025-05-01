module Common.Parser where

import Control.Monad
import GHC.Base hiding (some, many)
import Common.Tag
import Common.POS

type TaggedPOS = (Tag, POS)

-- A tag is one of those 36 Penn Treebank
-- tags. Pontuation and tags that contain
-- a $ are singled out because of finicky
-- haskell syntax and Read derivation
tag :: Parser Tag
tag = pontuation 
  <|> fmap (const PRPs) (string "PRP$")
  <|> fmap (const WPs) (string "WP$")
  <|> (Parser reads)

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
