module Common.Tag where

import Data.List (elemIndex)

data Tag = CC | CD | DT | EX | FW | IN | JJ | JJR   | JJS   | LS  | MD  | NN  | NNS | NNP | NNPS | PDT | POS | PRP | PRPs | RB  | RBR | RBS | RP  | SYM | TO  | UH  | VB  | VBD | VBG | VBN | VBP | VBZ | WDT | WP  | WPs | WRB | P | BOS | XX
  deriving (Read, Eq, Ord, Enum)

instance Show Tag where
  show CC = "CC"
  show CD = "CD"
  show DT = "DT"
  show EX = "EX"
  show FW = "FW"
  show IN = "IN"
  show JJ = "JJ"
  show JJR = "JJR"
  show JJS = "JJS"
  show LS = "LS"
  show MD = "MD"
  show NN = "NN"
  show NNS = "NNS"
  show NNP = "NNP"
  show NNPS = "NNPS"
  show PDT = "PDT"
  show POS = "POS"
  show PRP = "PRP"
  show PRPs = "PRP$"
  show RB = "RB"
  show RBR = "RBR"
  show RBS = "RBS"
  show RP = "RP"
  show  SYM = "SYM"
  show TO = "TO"
  show  UH = "UH"
  show  VB = "VB"
  show  VBD = "VBD"
  show VBG = "VBG"
  show VBN = "VBN"
  show VBP = "VBP"
  show VBZ = "VBZ"
  show WDT = "WDT"
  show WP = "WP"
  show  WPs = "WP$"
  show WRB = "WRB"
  show P   = "P"
  show BOS   = "BOS"
  show XX  = "XX"

toNat :: Tag -> Int
toNat t = 
  case elemIndex t [CC .. P] of
    Just x -> x
    Nothing -> undefined
