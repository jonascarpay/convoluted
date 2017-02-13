{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Layers.Convolution where

import Network
import Static
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Maybe
import Data.Serialize

-- | A convolution layer performs the correlation operation on its forward pass, and a convolution on the
--   backward pass.
data Convolution (od :: Nat) (id :: Nat) (kh :: Nat) (kw :: Nat) (oh :: Nat) (ow :: Nat) where
  Convolution :: ( KnownNat od, KnownNat id, KnownNat kh, KnownNat kw, KnownNat oh, KnownNat ow )
              => SArray U (ZZ ::. od ::. id ::. kh ::. kw)
              -> SArray U (ZZ ::. od        ::. oh ::. ow)
              -> Maybe (Gradient (Convolution od id kh kw oh ow))
              -> Convolution od id kh kw oh ow

instance ( KnownNat od, KnownNat oh, KnownNat ow, KnownNat id, KnownNat kh, KnownNat kw
         ) => Serialize (Convolution od id kh kw oh ow) where
  put (Convolution w b _) = do put w
                               put b

  get = do w <- get
           b <- get
           return$ Convolution w b Nothing

instance Show (Convolution od id kh kw oh ow) where
  show (Convolution w b _) = unlines ["Convolution", "Weights:", show w, "Bias:", show b]

instance ( KnownNat od , KnownNat id, KnownNat kh, KnownNat kw, KnownNat oh, KnownNat ow
         ) => Updatable (Convolution od id kh kw oh ow) where

  type Gradient (Convolution od id kh kw oh ow) =
    ( SArray U (ZZ ::. od ::. id ::. kh ::. kw)
    , SArray U (ZZ ::. od ::. oh ::. ow))

  zeroLayer = Convolution sZeros sZeros Nothing

  randomLayer seed =
    Convolution (sRandom seed (-1) 1) (sRandom (seed*9) (-1) 1) Nothing

  {-# INLINE applyDelta #-}
  applyDelta (LearningParameters α γ λ) (Convolution w b mVel) (dw, db) =
    do let (vw, vb) = fromMaybe (sZeros, sZeros) mVel

       vw' <- sComputeP$ sZipWith (\v d -> γ*v - α*d)     vw dw
       w'  <- sComputeP$ sZipWith (\w v -> w + v - λ * w) w  vw'

       vb' <- sComputeP$ sZipWith (\v d -> γ*v - α*d)     vb db
       b'  <- sComputeP$ sZipWith (\b v -> b + v - λ * b) b  vb'

       return $! Convolution w' b' (Just (vw', vb'))

instance
  ( KnownNat kh, KnownNat kw, KnownNat od, KnownNat id, KnownNat bat
  , KnownNat oh, KnownNat ow, KnownNat ih, KnownNat iw
  , KnownNat (kh :+ oh :- 1), KnownNat (kw :+ ow :- 1)
  , KnownNat (oh :+ 2 :* (kh :- 1))
  , KnownNat (ow :+ 2 :* (kw :- 1))
  , KnownNat (Halve ( ow :+ 2 :* (kw :- 1) :- ow ))
  , KnownNat (Halve ( oh :+ 2 :* (kh :- 1) :- oh ))
  , ih ~ (kh :+ oh :- 1)
  , iw ~ (kw :+ ow :- 1)
  , (kh :+ (kh :+ oh :- 1) :- 1) ~ (oh :+ (2 :* (kh :- 1)))
  , (kw :+ (kw :+ ow :- 1) :- 1) ~ (ow :+ (2 :* (kw :- 1)))
  ) => Layer (ZZ ::. bat ::. id ::. ih ::. iw)
             (Convolution od id kh kw oh ow) where


    type LOutput (ZZ ::. bat ::. id ::. ih ::. iw)
                 (Convolution od id kh kw oh ow) =
                 (ZZ ::. bat ::. od ::. oh ::. ow)

    runForward (Convolution w b _) x =
      sComputeP $ (w `corrB` x) %+ sExpand b

    runBackwards (Convolution w _ _) x _ dy =
      do dx <- sComputeP$ w `fullConvB` dy
         dw <- sComputeP$ dy `corrVolumesB` x
         db <- sComputeP$ sumOuter dy
         return ((dw, db), dx)

