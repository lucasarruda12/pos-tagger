module Main where

import Common.Parser
import Common.Tag
import Common.POS
import qualified Data.Map as Map
import Control.Exception (catch, IOException)
import Data.List (stripPrefix)

newtype UnigramTree = UT (Map.Map POS (Map.Map Tag Int))
  deriving (Show)

unknownThreshold = 1

add :: TaggedPOS -> UnigramTree -> UnigramTree
add (t, p) (UT m) = UT $ Map.insertWith (Map.unionWith (+)) p (Map.singleton t 1) m

addMany :: [TaggedPOS] -> UnigramTree -> UnigramTree
addMany (tp:tps) ut = add tp (addMany tps ut)
addMany [] ut       = ut

readUntilEOF :: UnigramTree -> IO UnigramTree
readUntilEOF ut = catch
  (do l   <- getLine 
      mt  <- pure (parse' line l)
      ut' <- pure $ addMany mt ut
      readUntilEOF ut')
  (\e -> do 
    let _ = e :: IOException
    pure ut)

toList :: UnigramTree -> [(POS, Int)]
toList (UT m) = Map.toList $ fmap ((foldr ((+) . snd) 0) . Map.toList) m

prune :: [(POS, Int)] -> [POS]
prune ((p, x) : ps)
  | x <= unknownThreshold = p : (prune ps)
  | otherwise = prune ps
prune [] = []

prettyPrint :: [String] -> String
prettyPrint (a:[])   = a
prettyPrint (a:as@(_:_)) = a ++ "\n" ++ prettyPrint as
prettyPrint [] = ""

replace :: String -> String -> String -> String 
replace [] to xs = go xs
    where go [] = to
          go (x:xs) = to ++ x : go xs
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []

replaceWithUnk :: String -> String -> String
replaceWithUnk s t = replace (" " ++ s ++ "_") " UNK_" t

main :: IO ()
main = do
  ut  <- readUntilEOF (UT Map.empty)
  p   <- pure (prune . toList $ ut) 
  putStrLn (prettyPrint p)
