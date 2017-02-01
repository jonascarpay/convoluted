{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Layers.Convolution where

import Volume
import Network
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Maybe

data Convolution (n :: Nat) (d :: Nat) (h :: Nat) (w :: Nat) where
  Convolution :: ( KnownNat n, KnownNat d, KnownNat h, KnownNat w )
              => { convW :: SArray U (ZZ ::. n ::. d ::. h ::. w)
                 , convB :: SArray U (ZZ ::. n ::. h ::. w)
                 , convV :: Maybe (Gradient (Convolution n d h w))
                 } -> Convolution n d h w

instance Show (Convolution n d h w) where
  show (Convolution w b _) = unlines ["Convolution", "Weights:", show w, "Biases:", show b]

instance ( KnownNat n , KnownNat d, KnownNat h, KnownNat w
         ) => Updatable (Convolution n d h w) where

  type Gradient (Convolution n d h w) = (SArray U (ZZ ::. n ::. d ::. h ::. w), SArray U (ZZ ::. n ::. h ::. w))
  seededRandom seed = Convolution (sRandom seed (-1) 1) (sRandom (seed^(9 :: Int)) (-1) 1) Nothing

  applyDelta (LearningParameters α γ λ) (Convolution w b mVel) (dw, db) =
    do let (vw, vb) = fromMaybe (sZeros, sZeros) mVel
       vw' <- sComputeP$ sZipWith (\v d -> γ*v - α*d) vw dw
       vb' <- sComputeP$ sZipWith (\v d -> γ*v - α*d) vb db
       w'  <- sComputeP$ sZipWith (\w v -> w + v - λ * w) w vw'
       b'  <- sComputeP$ sZipWith (\b v -> b + v - λ * b) b vb'
       return $! Convolution w' b' (Just (vw', vb'))

instance
  ( KnownNat kh, KnownNat kw, KnownNat n, KnownNat d
  , oh ~ (ih :- kh :+ 1)
  , ow ~ (iw :- kw :+ 1)
  ) => Layer (Convolution n d kh kw) (ZZ ::. d ::. ih ::. iw) (ZZ ::. n ::. oh ::. ow) where

    runForward (Convolution w b _) x = undefined
    runBackwards = undefined
