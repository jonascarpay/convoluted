{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
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
import Data.Proxy

data MultiSoftMax (cs :: [Nat]) = MultiSoftMax

instance Updatable (MultiSoftMax cs) where
  type Gradient (MultiSoftMax cs) = (MultiSoftMax cs)
  applyDelta _ _ _ = return MultiSoftMax
  seededRandom _ = MultiSoftMax

instance
  ( KnownNat bat, KnownNat o
  , SingI cs
  , o ~ Sum cs
  ) => Layer (MultiSoftMax cs) (ZZ ::. bat ::. o) where

  type InputShape (MultiSoftMax cs) (ZZ ::. bat ::. o) = (ZZ ::. bat ::. o)

  {-# INLINE runForward #-}
  runForward _ x = return vec'
    where
      cs = fromInteger <$> fromSing (sing :: Sing cs)
      vec' = sVectorMap (multiSoftMax cs) x

  {-# INLINE runBackwards #-}
  runBackwards _ _ (y :: SArray U (ZZ ::. bat ::. o)) dy =
    do let n = fromInteger $ natVal (Proxy :: Proxy bat)

       dx <- sComputeP . sReshape $ sZipWith (\y l -> (y-l)/n) y dy
       -- losses <- sSumAllP $ sMap (\x -> if x == 0 then 0 else -log x) $ y %* dy
       return (MultiSoftMax, dx)

instance
  ( KnownNat bat, KnownNat o
  , SingI cs
  , o ~ Sum cs
  ) => OutputLayer (MultiSoftMax cs) (ZZ ::. bat ::. o) (SArray U (ZZ ::. bat ::. o)) where

  {-# INLINE activate #-}
  activate _ x = vec'
    where
      cs   = fromInteger <$> fromSing (sing :: Sing cs)
      vec' = sVectorMap (multiSoftMax cs) x

  dError = undefined
