{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layers.MultiSoftMax where

import Network
import Util
import Static
import Data.Array.Repa as R
import Data.Singletons.TypeLits
import Data.Singletons.Prelude (Sing, SingI, fromSing, sing)
import Data.Singletons.Prelude.List (Sum)
import Data.Proxy
import Data.Serialize

data MultiSoftMax (cs :: [Nat]) = MultiSoftMax

instance Serialize (MultiSoftMax cs) where
  put _ = return ()
  get   = return MultiSoftMax

instance Creatable (MultiSoftMax cs) where
  seeded _ = MultiSoftMax

instance Updatable (MultiSoftMax cs) where
  type Gradient (MultiSoftMax cs) = ()
  applyDelta _ _ _ = return MultiSoftMax

instance
  ( KnownNat bat, KnownNat o
  , SingI cs
  , o ~ Sum cs
  ) => Layer (ZZ ::. bat ::. o) (MultiSoftMax cs) where

  type LOutput (ZZ ::. bat ::. o) (MultiSoftMax cs) = (ZZ ::. bat ::. o)
  {-# INLINE runForward #-}
  runForward _ x = return vec'
    where
      cs = fromInteger <$> fromSing (sing :: Sing cs)
      vec' = sVectorMap (multiSoftMax cs) x

  {-# INLINE runBackwards #-}
  runBackwards _ _ (y :: SArray U (ZZ ::. bat ::. o)) dy =
    do let n = fromInteger $ natVal (Proxy :: Proxy bat)

       dx <- sComputeP$ sZipWith (\y l -> (y-l)/n) y dy
       return ((), dx)

instance (KnownNat bat, Layer (ZZ ::. bat ::. o) (MultiSoftMax cs))
  => OutputLayer (ZZ ::. bat ::. o) (MultiSoftMax cs) where
  {-# INLINE runOutput #-}
  runOutput l x y =
    do fx      <- runForward l x
       c       <- numCorrect fx y
       (_, dx) <- runBackwards l x fx y
       return (dx, ((c, round n), dataLoss fx y))

   where
     !n = fromInteger$ natVal (Proxy :: Proxy bat)
     loss 0 = 0
     loss x = - log x
     dataLoss (SArray !f) (SArray !y) =
       (/n) . sumAllS $ R.zipWith (*) (R.map loss f) y

     numCorrect x y = do !s <- sSumAllP$ sZipWith (\ !x !y -> if x > 0.5 then y else 0) x y
                         return$! round s
