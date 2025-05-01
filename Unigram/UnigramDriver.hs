module Main where

import Common.Parser
import Common.Tag
import Common.POS
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad.State
import Control.Exception (catch, IOException)

newtype UnigramTree = UT (Map.Map POS (Map.Map Tag Int))
  deriving (Show)

add :: TaggedPOS -> UnigramTree -> UnigramTree
add (t, p) (UT m) = UT $ Map.insertWith (Map.unionWith (+)) p (Map.singleton t 1) m

addMany :: [TaggedPOS] -> UnigramTree -> UnigramTree
addMany (tp:tps) ut = add tp (addMany tps ut)
addMany [] ut = ut

readUntilEOF :: UnigramTree -> IO UnigramTree
readUntilEOF ut = catch
  (do l   <- getLine 
      mt  <- pure (parse' line l)
      ut' <- case mt of 
              Nothing -> pure ut
              (Just tps) -> pure $ addMany tps ut
      readUntilEOF ut')
  (\e -> do 
    let _ = e :: IOException
    pure ut)

larger :: (Tag, Int) -> (Tag, Int) -> (Tag, Int)
larger t1@(_,x) t2@(_,y) 
  | x > y     = t1
  | otherwise = t2

prune :: UnigramTree -> [(POS, Tag)]
prune (UT m) = Map.toList $ fmap (fst . takeLargest) m
  where 
    takeLargest :: Map.Map Tag Int -> (Tag, Int)
    takeLargest = (foldr larger (CC, 0)) . Map.toList

prettyPrintList :: [(POS, Tag)] -> String
prettyPrintList []          = ""
prettyPrintList ((a, b):xs) = a ++ " " ++ show b ++ "\n" ++ prettyPrintList xs

-- main = pure ()
main :: IO ()
main = do
  ut  <- readUntilEOF (UT Map.empty)
  p   <- pure (prune ut) 
  putStrLn (prettyPrintList p)
  
