{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Runners where

import Network
import Static

forward :: Monad m => Network i ls -> SArray U i -> m (SArray U (NOutput (Network i ls)))
forward (NNil l)       arr = runForward l arr
forward (l `NCons` ls) arr = do y <- runForward l arr
                                forward ls y

trainOnce :: Monad m
          => Network i ls
          -> LearningParameters
          -> SArray U i -- ^ Input
          -> SArray U (NOutput (Network i ls)) -- ^ Corresponding output
          -> m (Network i ls, Loss)
trainOnce net0 params x0 y0 = do (!net', !loss, _) <- go net0 x0 y0
                                 return (net', loss)
  where
    go :: Monad m
       => Network i ls
       -> SArray U i
       -> SArray U (NOutput (Network i ls))
       -> m ( Network i ls, Loss, SArray U i)
    go net@(NNil l) x y =
      do (dx, loss) <- runOutput l x y
         return (net, loss, dx)

    go (l `NCons` ls) x y =
      do f <- runForward l x
         (ls', loss, df) <- go ls f y
         (dl, dx) <- runBackwards l x f df
         l' <- applyDelta params l dl
         return (l' `NCons` ls', loss, dx)
