{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Layers.Convolution where

import Volume
import Network
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num

data Convolution (n :: Nat) (d :: Nat) (h :: Nat) (w :: Nat)
  = Convolution { weights :: SArray U (ZZ ::. n ::. d ::. h ::. w)
                , bias    :: SArray U (ZZ ::. n ::. h ::. w)
                }

instance Updatable (Convolution n d h w) where
  type Gradient (Convolution n d h w) = Convolution n d h w
  applyDelta _ _ _ = undefined
  createRandom     = undefined

instance
  ( oh ~ (ih :- kh :+ 1)
  , ow ~ (iw :- kw :+ 1)
  ) => Layer (Convolution n d kh kw) (ZZ ::. d ::. ih ::. iw) (ZZ ::. n ::. oh ::. ow) where

  runForward   = undefined
  runBackwards = undefined
