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
