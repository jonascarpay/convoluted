{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.Flatten where

import Network
import Static
import Util
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Serialize

data Flatten = Flatten

instance Serialize Flatten where
  put _ = return ()
  get   = return Flatten

instance Creatable Flatten where
  seeded _ = Flatten

instance Updatable Flatten where
  type Gradient Flatten = ()

instance ( KnownNat (h :* d :* w), KnownNat bat, KnownNat h, KnownNat d, KnownNat w
         , (bat :* h :* d :* w) ~ (bat :* (h :* d :* w))
         ) => Layer (ZZ ::. bat ::. h ::. d ::. w) Flatten where

  type LOutput (ZZ ::. bat ::. h ::. d ::. w) Flatten = ZZ ::. bat ::. (h :* d :* w)
  runForward _ arr = sComputeP $ sReshape arr

  runBackwards _ _ _ dy =
    do dx <- sComputeP $ sReshape dy
       return ((), dx)
