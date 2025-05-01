module Main where

import Common.Parser
import Common.POS
import Control.Exception (catch, IOException)

tester :: IO [POS]
tester = catch 
  (do 
    l <- getLine
    mt <- pure (parse' line l) 
    tps <- case mt of
      Nothing -> pure []
      Just tps -> pure tps
    pure $ fmap snd tps)
  (\e -> do 
    let _ = e :: IOException
    pure [])

main = pure ()
