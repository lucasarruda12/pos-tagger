module Common.Loop where

-- While condition holds, sequence action passing the result of one as input to the next.
iterateUntil :: Monad m => m Bool -> (a -> m a) -> a -> m a
iterateUntil mcond f input = do
  cond <- mcond
  if cond then do
    result <- f input 
    iterateUntil mcond f result
  else pure input

iterateUnless :: Monad m => m Bool -> (a -> m a) -> a -> m a
iterateUnless = iterateUntil . fmap not
