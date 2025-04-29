module Parser where

import Control.Monad
import GHC.Base hiding (some, many)
import PrimitiveParser
import Tag
import POS

type TaggedPOS = (Tag, POS)

-- O parser das Tags ta la embaixo pois eh enorme
tag :: Parser Tag
tag = pontuation <|> (Parser reads)

pontuation :: Parser Tag
pontuation = fmap (\x -> P) 
  (string "," <|> string "``" <|> string "." <|> string ":")

word :: Parser String
word = many (sat (/= '_'))

taggedpos :: Parser TaggedPOS
taggedpos = do
  w <- word
  _ <- char '_'
  t <- tag
  pure (t, w)

line :: Parser [TaggedPOS]
line = taggedpos `sepBy` char ' '


