module Runners where

import Network
import Volume

forward :: Monad m => Network i ls o -> SArray U i -> m (SArray U o)
forward (NNil l)       arr = runForward l arr
forward (l `NCons` ls) arr = do y <- runForward l arr
                                forward ls y
