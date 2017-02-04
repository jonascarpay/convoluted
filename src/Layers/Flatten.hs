{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Layers.Flatten where

import Network
import Volume
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num

data Flatten (d :: Nat) (h :: Nat) (w :: Nat) = Flatten

instance Updatable (Flatten d h w) where
  type Gradient (Flatten d h w) = Flatten d h w
  zeroLayer = Flatten

instance ( KnownNat bat, KnownNat n, KnownNat h, KnownNat w, KnownNat d
         , (d :* h :* w) ~ n
         , (bat :* d :* h :* w) ~ (bat :* n) -- 💁
         ) => Layer (Flatten d h w) (ZZ ::. bat ::. n) where

  type InputShape (Flatten d h w) (ZZ ::. bat ::. n) = ZZ ::. bat ::. d ::. h ::. w

  runForward _ arr = sComputeP $ sReshape arr

  runBackwards _ _ _ dy =
    do dx <- sComputeP $ sReshape dy
       return (Flatten, dx)
