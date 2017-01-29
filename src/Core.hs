{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Core where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Array.Repa

data SMeasure = ZZ | SMeasure ::. Nat
infixl 3 ::.

class Measure s where
  type ShapeOf s
  type Size s :: Nat
  mExtent :: p s -> ShapeOf s

instance Measure ZZ where
  type ShapeOf ZZ = Z
  type Size ZZ = 1
  mExtent _ = Z

instance (KnownNat n, Measure m) => Measure (m ::. n) where
  type ShapeOf (m ::. n) = (ShapeOf m) :. Int
  type Size (m ::. n) = Size m :* n
  mExtent _ = mExtent (undefined :: p m) :. fromInteger (natVal (undefined :: p n))
