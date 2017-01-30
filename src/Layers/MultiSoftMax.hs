{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layers.MultiSoftMax where

import Network
import Volume
import Data.Singletons.TypeLits
import Data.Singletons.Prelude (Sing, SingI, fromSing, sing)
import Data.Singletons.Prelude.List (Sum)

data MultiSoftMax (cs :: [Nat]) = MultiSoftMax

instance Updatable (MultiSoftMax cs) where
  type Gradient (MultiSoftMax cs) = (MultiSoftMax cs)
  applyDelta _ _ _ = return (MultiSoftMax, MultiSoftMax)
  createRandom = return MultiSoftMax

instance
  ( KnownNat i
  , SingI cs
  , i ~ Sum cs
  ) => Layer (MultiSoftMax cs) (ZZ ::. i) (ZZ ::. i) where

  {-# INLINE runForward #-}
  runForward _ x = return vec'
    where
      cs = fromInteger <$> fromSing (sing :: Sing cs)
      vec' = sVectorMap (multiSoftMax cs) x

  {-# INLINE runBackwards #-}
  runBackwards _ _ (y) (dy) =
    do dx <- sComputeP$ sZipWith (\y l -> (y-l)/1) y dy
       losses <- sSumAllP $ sMap (\x -> if x == 0 then 0 else -log x) $ y %* dy
       return (MultiSoftMax, dx, losses/1)
