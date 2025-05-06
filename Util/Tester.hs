module Main where

import Common.Parser
import Data.List (intercalate)
import System.IO

main = do 
  eof <- isEOF
  if eof then pure () else do
    getLine >>=                -- read from stdin
        pure . parse' line >>=     -- parse the line read
        pure . fmap snd >>=        -- remove the tags
        pure . intercalate " " >>= -- intercalate with spaces
        putStrLn                   -- write to stdout
    main -- and do it again!
