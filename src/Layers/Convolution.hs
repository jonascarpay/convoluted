{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Layers.Convolution where

import Volume
import Network
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Maybe

data Convolution (od :: Nat) (id :: Nat) (kh :: Nat) (kw :: Nat) (oh :: Nat) (ow :: Nat) where
  Convolution :: ( KnownNat od, KnownNat id, KnownNat kh, KnownNat kw, KnownNat oh, KnownNat ow )
              => SArray U (ZZ ::. od ::. id ::. kh ::. kw)
              -> SArray U (ZZ ::. od        ::. oh ::. ow)
              -> Maybe (Gradient (Convolution od id kh kw oh ow))
              -> Convolution od id kh kw oh ow

instance Show (Convolution od id kh kw oh ow) where
  show (Convolution w b _) = unlines ["Convolution", "Weights:", show w, "Bias:", show b]

instance ( KnownNat od , KnownNat id, KnownNat kh, KnownNat kw, KnownNat oh, KnownNat ow
         ) => Updatable (Convolution od id kh kw oh ow) where

  type Gradient (Convolution od id kh kw oh ow) =
    ( SArray U (ZZ ::. od ::. id ::. kh ::. kw)
    , SArray U (ZZ ::. od ::. oh ::. ow))

  seededRandom seed =
    Convolution (sRandom seed (-1) 1) (sRandom (seed*9) (-1) 1) Nothing

  applyDelta (LearningParameters α γ λ) (Convolution w b mVel) (dw, db) =
    do let (vw, vb) = fromMaybe (sZeros, sZeros) mVel

       vw' <- sComputeP$ sZipWith (\v d -> γ*v - α*d)     vw dw
       w'  <- sComputeP$ sZipWith (\w v -> w + v - λ * w) w  vw'

       vb' <- sComputeP$ sZipWith (\v d -> γ*v - α*d)     vb db
       b'  <- sComputeP$ sZipWith (\b v -> b + v - λ * b) b  vb'

       return $! Convolution w' b' (Just (vw', vb'))

instance
  ( KnownNat kh, KnownNat kw, KnownNat od, KnownNat id, KnownNat bat, KnownNat oh, KnownNat ow, KnownNat (kh :+ oh :- 1), KnownNat (kw :+ ow :- 1)
  ) => Layer (Convolution od id kh kw oh ow) (ZZ ::. bat ::. od ::. oh ::. ow) where

    type InputShape (Convolution od id kh kw oh ow) (ZZ ::. bat ::. od ::. oh ::. ow) =
      (ZZ ::. bat ::. id ::. (kh :+ oh :- 1) ::. (kw :+ ow :- 1))

    runForward (Convolution w b _) x =
      sComputeP $ (x `corrB` w) %+ sExpand b

    runBackwards (Convolution w b _) x _ dy =
      do dw <- sComputeP$ dy `corrVolumesB` x
         dx <- undefined
         db <- sComputeP$ sumOuter undefined
         return ((dw, db), dx, 0)


