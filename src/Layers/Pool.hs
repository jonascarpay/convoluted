{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.Pool where

import Network
import Volume
import Data.Singletons.TypeLits
import Data.Array.Repa

data Pool = Pool

instance Updatable Pool where
  type Gradient Pool = Pool
  zeroLayer          = Pool

instance ( KnownNat h, KnownNat h', KnownNat w, KnownNat w', KnownNat bat, KnownNat d
         , h' ~ (Halve h), w' ~ (Halve w)
         ) => Layer (ZZ ::. bat ::. d ::. h ::. w) Pool (ZZ ::. bat ::. d ::. h' ::. w') where

  runForward _ x = sComputeP$ sTraverse x f
    where f lx (b:.y:.x) = maximum [ lx$ b :. 2*y   :. 2*x
                                   , lx$ b :. 2*y+1 :. 2*x
                                   , lx$ b :. 2*y   :. 2*x+1
                                   , lx$ b :. 2*y+1 :. 2*x+1 ]

  runBackwards _ (SArray x) (SArray y) (SArray dy)
    = do dx <- sComputeP$ sFromFunction f
         return (Pool, dx)
      where
        halve (b:.y:.x) = b:. y `div` 2 :. x `div` 2
        f pos
          | x ! pos == y ! halve pos = dy ! halve pos
          | otherwise                = 0
