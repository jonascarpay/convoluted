{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Static.Measure where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Array.Repa
import Data.Proxy

data SMeasure = ZZ | SMeasure ::. Nat
infixl 3 ::.

class (Show (ShapeOf s), Shape (ShapeOf s)) => Measure (s :: SMeasure) where
  type Size               s :: Nat
  type Prepend (n :: Nat) s :: SMeasure
  mExtent :: p s -> ShapeOf s

instance Measure ZZ where
  type Size      ZZ = 1
  type Prepend n ZZ = ZZ ::. n
  mExtent _ = Z

instance (KnownNat n, Measure m) => Measure (m ::. n) where
  type Size       (m ::. n) = Size m :* n
  type Prepend n' (m ::. n) = Prepend n' m ::. n
  mExtent _ = mExtent (Proxy :: Proxy m) :. fromInteger (natVal (Proxy :: Proxy n))


type family (a :: SMeasure) :<> (b :: SMeasure) :: SMeasure
type instance ZZ          :<> q           = q
type instance q           :<> ZZ          = q
type instance (m1 ::. n1) :<> (m2 ::. n2) = ((m1 ::. n1) :<> m2) ::. n2

type family ShapeOf (s :: SMeasure) :: *
type instance ShapeOf ZZ = Z
type instance ShapeOf (m ::. n) = ShapeOf m :. Int

class (Measure a, Measure b, Expand (ShapeOf a) (ShapeOf b)) => a `Suffix` b
instance (KnownNat n, a  `Suffix` b) => (a ::. n) `Suffix` (b ::. n)
instance Measure b => ZZ `Suffix` b

class Expand small big where
  expand :: big -> small

instance Expand small big => Expand (small :. n) (big :. n) where
  {-# INLINE expand #-}
  expand (b :. n) = expand b :. n

instance Expand Z big where
  {-# INLINE expand #-}
  expand _ = Z

