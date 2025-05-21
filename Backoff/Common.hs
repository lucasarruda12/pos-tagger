module Backoff.Common where

import qualified Data.Map as Map
import Common.Parser
import System.IO

type Assoc a b = Map.Map a b

emptyAssoc :: Assoc a b
emptyAssoc = Map.empty

add :: Ord a => Assoc a b -> (a, b) -> Assoc a b
add ass (a, b) = Map.insert a b ass

lookupA :: Ord a => a -> Assoc a b -> Maybe b
lookupA = Map.lookup

populate :: Ord a => Handle -> Parser (a, b) -> Assoc a b -> IO (Assoc a b)
populate handle p ass = do
  eof <- hIsEOF handle
  if eof
  then pure ass else
      hGetLine handle >>=
      pure . parse' p >>=
      pure . add ass  >>=
      populate handle p

