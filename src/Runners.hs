{-# LANGUAGE DataKinds #-}

module Runners where

import Network
import Static

forward :: Monad m => Network i ls o -> SArray U i -> m (SArray U o)
forward (NNil l)       arr = runForward l arr
forward (l `NCons` ls) arr = do y <- runForward l arr
                                forward ls y

trainOnce :: Monad m
          => Network i ls o
          -> LearningParameters
          -> SArray U i -- ^ Input
          -> SArray U o -- ^ Corresponding output
          -> m (Network i ls o, Double)
trainOnce net params x y = do (net', loss, _) <- go net x y
                              return (net', loss)
  where
    go :: Monad m
       => Network i ls o
       -> SArray U i
       -> SArray U o
       -> m ( Network i ls o, Double, SArray U i)
    go net@(NNil l) x y =
      do (dx, loss) <- runOutput l x y
         return (net, loss, dx)

    go (l `NCons` ls) x y =
      do f <- runForward l x
         (ls', loss, df) <- go ls f y
         (dl, dx) <- runBackwards l x f df
         l' <- applyDelta params l dl
         return (l' `NCons` ls', loss, dx)
