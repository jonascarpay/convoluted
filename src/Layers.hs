{-# LANGUAGE GADTs #-}
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
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List

data MultiSoftMax (cs :: [Nat]) = MultiSoftMax

instance (Sum cs ~ Prod s) => OutputLayer (MultiSoftMax cs) s where
  -- runOutput :: Monad m => MultiSoftMax -> Volume i -> (Vector o Probs)
  runOutput _ vol@(Vol x) = do
    undefined

class KnownNats (a :: [Nat]) where
  natVals :: p a -> [Integer]

instance KnownNats '[] where
  natVals _ = []

instance (KnownNats xs, KnownNat x) => KnownNats (x ': xs) where
  natVals _ = natVal (undefined :: p x) : natVals (undefined :: p xs)
