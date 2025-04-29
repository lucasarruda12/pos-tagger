module Main where

import PrimitiveParser
import Parser 

main :: IO ()
main = do
  l <- getLine  
  putStrLn (show $ parse line l)
  main
