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

class Measure (s :: SMeasure) where
  type ShapeOf            s :: *
  type Size               s :: Nat
  type Prepend (n :: Nat) s :: SMeasure
  mExtent :: p s -> ShapeOf s

instance Measure ZZ where
  type ShapeOf   ZZ = Z
  type Size      ZZ = 1
  type Prepend n ZZ = ZZ ::. n
  mExtent _ = Z

instance (KnownNat n, Measure m) => Measure (m ::. n) where
  type ShapeOf    (m ::. n) = (ShapeOf m) :. Int
  type Size       (m ::. n) = Size m :* n
  type Prepend n' (m ::. n) = Prepend n' m ::. n
  mExtent _ = mExtent (undefined :: p m) :. fromInteger (natVal (undefined :: p n))

type family (a :: SMeasure) :<> (b :: SMeasure) :: SMeasure
type instance ZZ          :<> q           = q
type instance q           :<> ZZ          = q
type instance (m1 ::. n1) :<> (m2 ::. n2) = ((m1 ::. n1) :<> m2) ::. n2
