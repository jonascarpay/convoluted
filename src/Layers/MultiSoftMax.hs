{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layers.MultiSoftMax where

import Network
import Volume
import Data.Array.Repa as R
import Data.Singletons.TypeLits
import Data.Singletons.Prelude (Sing, SingI, fromSing, sing)
import Data.Singletons.Prelude.List (Sum)
import Data.Proxy

data MultiSoftMax (cs :: [Nat]) = MultiSoftMax

instance Updatable (MultiSoftMax cs) where
  type Gradient (MultiSoftMax cs) = (MultiSoftMax cs)
  applyDelta _ _ _ = return MultiSoftMax
  randomLayer _    = MultiSoftMax
  zeroLayer        = MultiSoftMax

instance
  ( KnownNat bat, KnownNat o
  , SingI cs
  , o ~ Sum cs
  ) => Layer (ZZ ::. bat ::. o) (MultiSoftMax cs) (ZZ ::. bat ::. o) where

  {-# INLINE runForward #-}
  runForward _ x = return vec'
    where
      cs = fromInteger <$> fromSing (sing :: Sing cs)
      vec' = sVectorMap (multiSoftMax cs) x

  {-# INLINE runBackwards #-}
  runBackwards _ _ (y :: SArray U (ZZ ::. bat ::. o)) dy =
    do let n = fromInteger $ natVal (Proxy :: Proxy bat)

       dx <- sComputeP . sReshape $ sZipWith (\y l -> (y-l)/n) y dy
       return (MultiSoftMax, dx)

instance Layer i (MultiSoftMax cs) o => OutputLayer i (MultiSoftMax cs) o where
  {-# INLINE runOutput #-}
  runOutput l x y =
    do fx      <- runForward l x
       dy      <- sComputeP$ fx %- y
       (_, dx) <- runBackwards l x fx dy
       return (dx, dataLoss fx y)

   where
     loss 0 = 0
     loss x = - log x
     dataLoss (SArray f) (SArray y) =
       sumAllS . R.map loss $ R.zipWith (*) f y
