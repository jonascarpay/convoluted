{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}

module Layers.Pool where

import Network
import Util
import Static
import Data.Singletons.TypeLits
import Data.Array.Repa
import Data.Serialize

data Pool = Pool

instance Serialize Pool where
  put _ = return ()
  get   = return Pool

instance Creatable Pool where
  seeded _ = Pool

instance Updatable Pool where
  type Gradient Pool = ()

instance ( KnownNat h, KnownNat (Halve h), KnownNat w, KnownNat (Halve w), KnownNat bat, KnownNat d
         ) => Layer (ZZ ::. bat ::. d ::. h ::. w) Pool where

  type LOutput (ZZ ::. bat ::. d ::. h ::. w) Pool =
    (ZZ ::. bat ::. d ::. Halve h ::. Halve w)

  runForward _ x = sComputeP$ sTraverse x f
    where f lx (b:.y:.x) = maximum [ lx$ b :. 2*y   :. 2*x
                                   , lx$ b :. 2*y+1 :. 2*x
                                   , lx$ b :. 2*y   :. 2*x+1
                                   , lx$ b :. 2*y+1 :. 2*x+1 ]

  runBackwards _ (SArray x) (SArray y) (SArray dy)
    = do dx <- sComputeP$ sFromFunction f
         return ((), dx)
      where
        halve (b:.y:.x) = b:. y `div` 2 :. x `div` 2
        f pos
          | x ! pos == y ! halve pos = dy ! halve pos
          | otherwise                = 0
