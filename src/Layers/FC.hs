{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.FC where

import Network
import Static
import Data.Maybe
import Data.Singletons.TypeLits
import Data.Serialize

data FC (i :: Nat) (o :: Nat) where
  FC :: SArray U (ZZ ::. i ::. o)
     -> SArray U (ZZ ::. o)
     -> Maybe (Gradient (FC i o))
     -> FC i o

instance ( KnownNat i, KnownNat o
         ) => Serialize (FC i o) where
  put (FC w b _) = do put w
                      put b

  get            = do w <- get
                      b <- get
                      return$ FC w b Nothing

instance ( KnownNat i, KnownNat o
         ) => Creatable (FC i o) where

  seeded seed = FC (seeded seed) (seeded seed) Nothing

instance ( KnownNat i, KnownNat o
         ) => Updatable (FC i o) where

  type Gradient (FC i o) = (SArray U (ZZ ::. i ::. o), SArray U (ZZ ::. o))

  applyDelta (LearningParameters α γ λ) (FC w b mVel) (dw, db) =
    do let (vw,vb) = fromMaybe (sZeros, sZeros) mVel

       vw' <- sComputeP$ sZipWith (\v d -> γ*v - α*d) vw dw
       vb' <- sComputeP$ sZipWith (\v d -> γ*v - α*d) vb db

       w'  <- sComputeP$ sZipWith (\w v -> w+v - λ*w) w  vw'
       b'  <- sComputeP$ sZipWith (\b v -> b+v - λ*b) b  vb'

       return $! FC w' b' (Just (vw', vb'))

instance ( KnownNat bat, KnownNat i, KnownNat o
         ) => Layer (ZZ ::. bat ::. i) (FC i o) where

  type LOutput (ZZ ::. bat ::. i) (FC i o) = ZZ ::. bat ::. o
  runForward (FC w b _) x = sComputeP$ (x `smmMult` w) %+ sExpand b

  runBackwards (FC w _ _) x _ dy =
    do dw <- sComputeP$ sTranspose x `smmMult` dy
       db <- sComputeP$ sumOuter' dy
       dx <- sComputeP$ dy `smmMult` sTranspose w
       return ((dw, db), dx)
