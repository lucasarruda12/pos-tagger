module Common.Functions where

import Control.Monad (when)

whileM :: Monad m => m (Bool) -> (a -> m a) -> a -> m a
whileM mcond f state = do
  cond <- mcond 
  if cond
  then do
    state' <- f state
    whileM mcond state' f
  else pure state 
    
