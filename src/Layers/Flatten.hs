{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.Flatten where

import Network
import Volume
import Data.Singletons.TypeLits

data Flatten = Flatten

instance Updatable Flatten where
  type Gradient Flatten = Flatten
  zeroLayer = Flatten

instance ( KnownNat n, KnownNat bat, KnownNat h, KnownNat d, KnownNat w
         , Size (ZZ ::. bat ::. h ::. d ::. w) ~ Size (ZZ ::. bat ::. n)
         ) => Layer (ZZ ::. bat ::. h ::. d ::. w) Flatten (ZZ ::. bat ::. n) where

  runForward _ arr = sComputeP $ sReshape arr

  runBackwards _ _ _ dy =
    do dx <- sComputeP $ sReshape dy
       return (Flatten, dx)
