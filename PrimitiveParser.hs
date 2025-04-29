module PrimitiveParser where

import Control.Monad
import GHC.Base hiding (some, many)
 
-- My parser type
newtype Parser a = Parser {
  parse :: String -> [(a, String)]
}

-- Allows me to write pretty do-notation
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

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then pure x else empty

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string "" = pure ""
string (c : s) = do
  x <- char c
  xs <- string s
  pure (x : xs)

many :: Parser a -> Parser [a]
many p = some p <|> pure []

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  pure (x:xs) 

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p >>= \x -> many (sep >> p) >>= \xs -> return (x:xs)) <|> pure []
