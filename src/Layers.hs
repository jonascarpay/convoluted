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
import Core
import Data.Array.Repa as R
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List

data MultiSoftMax (cs :: [Nat]) = MultiSoftMax

instance (SingI cs, SingI (Sum cs), Sum cs ~ Prod s) => OutputLayer (MultiSoftMax cs) s where

  runOutput _ (Vol x) = return vec'
    where
      Z:.n:.d:.h:.w = extent x
      vec = R.toUnboxed x
      cs = fromInteger <$> fromSing (sing :: Sing cs)
      c' = fromInteger $ fromSing (sing :: Sing (Sum cs))
      vec' :: Vector (Sum cs) Probs = Vec . R.fromUnboxed (Z:.n:.c') $ multiSoftMax cs vec
