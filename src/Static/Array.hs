{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Static.Array
  ( module Static.Array
  , module Static.Measure
  , U, D
  ) where

import Static.Measure
import Data.Array.Repa                      as R
import Data.Array.Repa.Algorithms.Randomish as R
import Data.Monoid
import Data.Proxy
import qualified Data.Vector.Unboxed as U

newtype SArray r (s :: SMeasure) = SArray (R.Array r (ShapeOf s) Double)
instance Measure s => Show (SArray U s) where
  show (SArray arr) = "Static " <> show arr
instance Measure s => Show (SArray D s) where
  show (SArray arr) = "Delayed Static " <> show (computeS arr :: R.Array U (ShapeOf s ) Double)

{-# INLINE sFromFunction #-}
sFromFunction :: forall s. Measure s => (ShapeOf s -> Double) -> SArray D s
sFromFunction f = SArray $ fromFunction sh f
  where
    sh = mExtent (Proxy :: Proxy s)

{-# INLINE sZipWith #-}
sZipWith :: (Measure s, Source r1 Double, Source r2 Double)
         => (Double -> Double -> Double)
         -> SArray r1 s
         -> SArray r2 s
         -> SArray D s
sZipWith f (SArray arr1) (SArray arr2) = SArray $ R.zipWith f arr1 arr2

{-# INLINE sMap #-}
sMap :: (Source r Double, Measure s)
     => (Double -> Double)
     -> SArray r s
     -> SArray D s
sMap f (SArray arr) = SArray $ R.map f arr

{-# INLINE sComputeP #-}
sComputeP :: (Monad m, Measure s)
          => SArray D s
          -> m (SArray U s)
sComputeP (SArray arr) = SArray <$> computeP arr

{-# INLINE sComputeS #-}
sComputeS :: Measure s
          => SArray D s
          -> SArray U s
sComputeS (SArray arr) = SArray $ computeS arr

{-# INLINE (%*) #-}
{-# INLINE (%+) #-}
{-# INLINE (%-) #-}
{-# INLINE (%/) #-}
(%*), (%+), (%-), (%/) :: (Measure s, Source r2 Double, Source r1 Double)
     => SArray r1 s
     -> SArray r2 s
     -> SArray D  s
a %* b = sZipWith (*) a b
a %/ b = sZipWith (/) a b
a %+ b = sZipWith (+) a b
a %- b = sZipWith (-) a b

{-# INLINE sSumAllP #-}
sSumAllP :: (Source r Double, Measure s, Monad m)
         => SArray r s
         -> m Double
sSumAllP (SArray a) = sumAllP a

{-# INLINE sSumAllS #-}
sSumAllS :: (Source r Double, Measure s)
         => SArray r s
         -> Double
sSumAllS (SArray a) = sumAllS a

-- | Watch out: fromUnboxed, and sbFromUnboxed do not perform length checks.
--   You are advised to use sMapVector
{-# INLINE sFromUnboxed #-}
sFromUnboxed :: forall s.Measure s => U.Vector Double -> SArray U s
sFromUnboxed vec
  | size sh == U.length vec = arr
  | otherwise               = error "Unboxed vector length did not match target array size"
  where sh = mExtent (Proxy :: Proxy s)
        arr = SArray $ fromUnboxed sh vec


{-# INLINE sVectorMap #-}
sVectorMap :: Measure s
           => (U.Vector Double -> U.Vector Double)
           -> SArray U s
           -> SArray U s
sVectorMap vf (SArray arr)
  | U.length vec == U.length vec' = (sFromUnboxed vec')
  | otherwise                     = error "Vector function did not preserve length"
  where
    vec = toUnboxed arr
    vec' = vf vec

{-# INLINE sReshape #-}
sReshape :: forall r s1 s2.
          ( Source r Double
          , Size s1 ~ Size s2 -- GHC says this is redundant, GHC is wrong.
          , Measure s1
          , Measure s2
          )
          => SArray r s1
          -> SArray D s2
sReshape (SArray x) = SArray $ reshape sh x
  where
    sh = mExtent (Proxy :: Proxy s2)

sRandom :: forall s. Measure s => Int -> Double -> Double -> SArray U s
sRandom seed min max = SArray $ R.randomishDoubleArray sh min max seed
  where
    sh = mExtent (Proxy :: Proxy s)

sZeros :: forall s. Measure s => SArray U s
sZeros = SArray . computeS $ fromFunction sh (const 0)
  where
    sh = mExtent (Proxy :: Proxy s)

{-# INLINE sExpand #-}
sExpand :: forall sml big r1. (sml `Suffix` big, Source r1 Double)
        => SArray r1 sml
        -> SArray D  big
sExpand (SArray src) = SArray $ backpermute sh expand src
  where sh = mExtent (Proxy :: Proxy big)

{-# INLINE sBackpermute #-}
sBackpermute :: forall s1 s2 r. (Source r Double, Measure s1, Measure s2)
             => (ShapeOf s2 -> ShapeOf s1)
             -> SArray r s1
             -> SArray D s2
sBackpermute f (SArray arr) = SArray$ backpermute sh f arr
  where sh = mExtent (Proxy :: Proxy s2)

{-# INLINE sTraverse #-}
sTraverse :: forall s1 s2 r.
             (Source r Double, Measure s1, Measure s2)
          => SArray r s1
          -> ((ShapeOf s1 -> Double) -> ShapeOf s2 -> Double)
          -> SArray D s2
sTraverse (SArray arr) f = SArray$ R.traverse arr (const sh) f
  where sh = mExtent (Proxy :: Proxy s2)
