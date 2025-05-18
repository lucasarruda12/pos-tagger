module Bigram.Common where

import Common.POS

data Bigram = Bigram {
  predict :: POS,
  given :: POS
} deriving (Eq, Ord)

instance Show Bigram where
  show (Bigram pred given) = pred ++ "_" ++ given
