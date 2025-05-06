-- I wanted to find out if i had any errors
-- when parsing the lines of the test set.
-- The ony thing this does is tell me if
-- a line was succefully parsed.
module Main where

import Common.Parser

tryParseCount :: Int -> IO ()
tryParseCount x = do
  l <- getLine
  ml <- pure $ parse' line l
  case ml of
    Nothing -> putStrLn $ "- " ++ show x
    (Just _) -> putStrLn $ "+ " ++ show x
  tryParseCount (x + 1)

main :: IO ()
main = do
  tryParseCount 1
