{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layers where

import Network
import Volume
import Data.Array.Repa as R
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List

data MultiSoftMax (cs :: [Nat]) = MultiSoftMax

instance Updatable (MultiSoftMax cs) where
  type Gradient (MultiSoftMax cs) = (MultiSoftMax cs)
  applyDelta _ _ _ = return (MultiSoftMax, MultiSoftMax)
  createRandom = return MultiSoftMax

instance
  ( Shape (ShapeOf i)
  , SingI cs
  , SingI (Sum cs)
  , Size i ~ Sum cs
  , Sum cs ~ o
  ) => Layer (MultiSoftMax cs) i (ZZ ::. o) where

  runForward _ (SBatch x) = return vec'
    where
      n = last . listOfShape . extent $ x
      vec = R.toUnboxed x
      cs = fromInteger <$> fromSing (sing :: Sing cs)
      c' = fromInteger  $  fromSing (sing :: Sing (Sum cs))
      vec' = SBatch . R.fromUnboxed (Z:.n:.c') $ multiSoftMax cs vec

  runBackwards _ _ _ = undefined
