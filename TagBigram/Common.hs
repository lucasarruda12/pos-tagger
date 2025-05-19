module TagBigram.Common where

import Common.POS
import Common.Tag

data Bigram = Bigram {
  predict :: POS,
  give :: Tag
} deriving (Eq, Ord)

instance Show Bigram where
  show (Bigram pred given) = pred ++ "_" ++ show given
