module Main where

import Common.Parser
import Common.POS
import Control.Exception (catch, IOException)
import Control.Monad (unless)
import Data.List (intercalate)

import System.IO

loop :: (String -> String) -> IO ()
loop f = do
  eof <- isEOF
  unless eof (do 
    getLine >>= (putStrLn . f)
    loop f)

main = loop $ 
  (intercalate " ") . (fmap snd) . (parse' line)
