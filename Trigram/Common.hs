module Trigram.Common where

import Common.POS

data Trigram = Trigram {
  predict :: POS,
  given1 :: POS,
  given2 :: POS
} deriving (Eq, Ord)

instance Show Trigram where
  show (Trigram pred g1 g2) = pred ++ "_" ++ g1 ++ "_" ++ g2
