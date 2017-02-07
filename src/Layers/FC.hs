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

data FC (i :: Nat) (o :: Nat) where
  FC :: SArray U (ZZ ::. i ::. o)
     -> SArray U (ZZ ::. o)
     -> Maybe (Gradient (FC i o))
     -> FC i o

instance ( KnownNat i, KnownNat o
         ) => Updatable (FC i o) where

  type Gradient (FC i o) = (SArray U (ZZ ::. i ::. o), SArray U (ZZ ::. o))

  zeroLayer = FC sZeros sZeros Nothing

  randomLayer seed = FC (sRandom seed 0 1) (sRandom (seed*9) 0 1) Nothing

  applyDelta (LearningParameters α γ λ) (FC w b mVel) (dw, db) =
    do let (vw,vb) = fromMaybe (sZeros, sZeros) mVel

       vw' <- sComputeP$ sZipWith (\v d -> γ*v - α*d) vw dw
       w'  <- sComputeP$ sZipWith (\w v -> w+v - λ*w) w  vw'
       vb' <- sComputeP$ sZipWith (\v d -> γ*v - α*d) vb db
       b'  <- sComputeP$ sZipWith (\b v -> b+v - λ*b) b  vb'
       return $! FC w' b' (Just (vw', vb'))

instance ( KnownNat bat, KnownNat i, KnownNat o
         ) => Layer (ZZ ::. bat ::. i) (FC i o) (ZZ ::. bat ::. o) where

  runForward (FC w b _) x = sComputeP$ (x `smmMult` w) %+ sExpand b

  runBackwards (FC w _ _) x _ dy =
    do dw <- sComputeP$ sTranspose x `smmMult` dy
       db <- sComputeP$ sumOuter' dy
       dx <- sComputeP$ dy `smmMult` sTranspose w
       return ((dw, db), dx)
