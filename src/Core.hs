{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Core where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Array.Repa

data Size = S1 Nat
          | S2 Nat Nat
          | S3 Nat Nat Nat
          | S4 Nat Nat Nat Nat

class Measure (a :: Size) where
  type RepaShape a
  type Mass a :: Nat
  sExtent :: p a -> RepaShape a

instance KnownNat w => Measure (S1 w) where
  type RepaShape (S1 w) = DIM1
  type Mass      (S1 w) = w
  sExtent _ = let w = fromInteger $ natVal (undefined :: p1 w)
               in Z:.w

instance (KnownNat h, KnownNat w) => Measure (S2 h w) where
  type RepaShape (S2 h w) = DIM2
  type Mass      (S2 h w) = h :* w
  sExtent _ = let w = fromInteger $ natVal (undefined :: p1 w)
                  h = fromInteger $ natVal (undefined :: p2 h)
               in Z:.h:.w

instance (KnownNat d, KnownNat h, KnownNat w) => Measure (S3 d h w) where
  type RepaShape (S3 d h w) = DIM3
  type Mass      (S3 d h w) = d :* h :* w
  sExtent _ = let w = fromInteger $ natVal (undefined :: p1 w)
                  h = fromInteger $ natVal (undefined :: p2 h)
                  d = fromInteger $ natVal (undefined :: p3 d)
               in Z:.d:.h:.w

instance (KnownNat n, KnownNat d, KnownNat h, KnownNat w) => Measure (S4 n d h w) where
  type RepaShape (S4 n d h w) = DIM4
  type Mass      (S4 n d h w) = n :* d :* h :* w
  sExtent _ = let w = fromInteger $ natVal (undefined :: p1 w)
                  h = fromInteger $ natVal (undefined :: p2 h)
                  d = fromInteger $ natVal (undefined :: p3 d)
                  n = fromInteger $ natVal (undefined :: p4 n)
               in Z:.n:.d:.h:.w
