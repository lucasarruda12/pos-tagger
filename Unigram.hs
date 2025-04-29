module Main where

import Parser
import PrimitiveParser
import Tag
import Data.Map as Map
import Data.List (intercalate)
import Control.Monad.State
import Control.Exception (catch, IOException)

newtype UnigramTree = UT (Map.Map String (Map.Map Tag Int))

instance Show UnigramTree where
  show (UT outerMap) = intercalate "\n" $
    concatMap printEntry (Map.toList outerMap)
    where
      printEntry :: (String, Map Tag Int) -> [String]
      printEntry (str, tagMap) =
        (str ++ ":") : fmap (\(tag, count) -> "-> " ++ show tag ++ ": " ++ show count) (Map.toList tagMap)

add :: UnigramTree -> String -> Tag -> UnigramTree
add (UT m) s t = case Map.lookup s m of
  Nothing -> UT $ Map.insert s (Map.singleton t 1) m
  Just m' -> UT $ Map.insert s (Map.adjust (+1) t m') m

populate :: UnigramTree -> Maybe [TaggedPOS] -> UnigramTree
populate ut (Just ((t, p):xs))  = add (populate ut (Just xs)) p t
populate ut _                   = ut

readAndPopulate :: UnigramTree -> IO UnigramTree
readAndPopulate ut =
  getLine >>= pure . (populate ut) . (parse' line)

loopUntilEOF :: UnigramTree -> IO UnigramTree
loopUntilEOF ut = catch
  (do ut' <- readAndPopulate ut
      loopUntilEOF ut')
  (\e -> do 
    let _ = e :: IOException
    pure ut)

-- main = pure ()
main :: IO ()
main = do
  ut <- loopUntilEOF (UT empty)
  putStrLn (show ut)
  
