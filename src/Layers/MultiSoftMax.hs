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
  ( KnownNat o
  , SingI cs
  , Size i ~ Sum cs
  , Size i ~ o
  , Measure i
  ) => Layer (MultiSoftMax cs) i (ZZ ::. o) where

  {-# INLINE runForward #-}
  runForward _ x = return vec'
    where
      cs = fromInteger <$> fromSing (sing :: Sing cs)
      vec' = sVectorMap (multiSoftMax cs) x

  {-# INLINE runBackwards #-}
  runBackwards _ _ (y :: SBatch U n (ZZ ::. o)) dy =
    do let n = fromInteger $ natVal (proxy :: p n)

       dx <- sComputeP . sReshape $ sZipWith (\y l -> (y-l)/n) y dy
       losses <- sSumAllP $ sMap (\x -> if x == 0 then 0 else -log x) $ y %* dy
       return (MultiSoftMax, dx, losses/n)
