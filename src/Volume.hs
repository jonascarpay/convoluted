{-# OPTIONS_GHC -Odph -rtsopts -threaded -fno-liberate-case -fllvm -optlo-O3
                -funfolding-use-threshold1000 -funfolding-keeness-factor1000 #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

-- This module is definitely not as elegant as it could be, with arrays
-- and batches having separate functions for the same operation. I will
-- optimise this later when I can keep an eye on the performance impact
-- of said optimizations

module Volume
  ( module Volume
  , module Measure
  , D, U
  )where

import Measure
import Data.Monoid ((<>))
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Array.Repa                      as R
import Data.Array.Repa.Algorithms.Randomish as R
import qualified Data.Vector.Unboxed as U
import Data.Proxy

-- | Volume and Vector hold data that is transferred betweeen layers.
--   For now, these both contain multiple rows of the same data, i.e.
--   are actually a data batch instead of a single sample. These might
--   be promoted to type classes later to accomodate both batches and
--   samples, or different precision data types for working on a GPU.

newtype SArray r            (s :: SMeasure) = SArray (R.Array r (ShapeOf    s) Double)
instance Measure s => Show (SArray U s) where
  show (SArray arr) = "Batch " <> show arr
instance Measure s => Show (SArray D s) where
  show (SArray arr) = "Delayed Static " <> show (computeS arr :: R.Array U (ShapeOf s ) Double)

newtype SBatch r (n :: Nat) (s :: SMeasure) = SBatch (R.Array r (ShapeOf' n s) Double)
instance Measure' n s => Show (SBatch D n s) where
  show (SBatch arr) = "Delayed Batch "  <> show (computeS arr :: R.Array U (ShapeOf' n s) Double)
instance Measure' n s => Show (SBatch U n s) where
  show (SBatch arr) = "Batch " <> show arr

{-# INLINE softMax #-}
softMax :: U.Vector Double -> U.Vector Double
softMax xs = U.map (/expSum) exps
  where xMax   = U.maximum xs
        exps   = U.map (\x -> exp $ x - xMax) xs
        expSum = U.sum exps

-- | Apply softmax to slices of the vector of lengths indicated by the list
{-# INLINE multiSoftMax #-}
multiSoftMax :: [Int] -> U.Vector Double -> U.Vector Double
multiSoftMax ls xs = U.concat $ sms (cycle ls) xs
  where
    sms (l:ls) xs | U.null xs = []
                  | U.length xs < l = undefined
                  | otherwise = let (h,t) = U.splitAt l xs
                                 in softMax h : sms ls t
    sms [] _ = undefined

{-# INLINE sFromFunction #-}
sFromFunction :: forall s. Measure s => (ShapeOf s -> Double) -> SArray D s
sFromFunction f = SArray $ fromFunction sh f
  where
    sh = mExtent (Proxy :: Proxy s)

{-# INLINE sbFromFunction #-}
sbFromFunction :: forall s n. Measure' n s => (ShapeOf' n s -> Double) -> SBatch D n s
sbFromFunction f = SBatch $ fromFunction sh f
  where
    sh = mExtent (Proxy :: Proxy (Prepend n s))

{-# INLINE sbZipWith #-}
sbZipWith :: (Measure' n s, Source r1 Double, Source r2 Double)
         => (Double -> Double -> Double)
         -> SBatch r1 n s
         -> SBatch r2 n s
         -> SBatch D n s
sbZipWith f (SBatch arr1) (SBatch arr2) = SBatch $ R.zipWith f arr1 arr2

{-# INLINE sZipWith #-}
sZipWith :: (Measure s, Source r1 Double, Source r2 Double)
         => (Double -> Double -> Double)
         -> SArray r1 s
         -> SArray r2 s
         -> SArray D s
sZipWith f (SArray arr1) (SArray arr2) = SArray $ R.zipWith f arr1 arr2


{-# INLINE sMap #-}
sMap :: (Source r Double, Measure' n s)
     => (Double -> Double)
     -> SBatch r n s
     -> SBatch D n s
sMap f (SBatch arr) = SBatch $ R.map f arr

{-# INLINE sbComputeP #-}
sbComputeP :: (Monad m, Measure' n s)
          => SBatch D n s
          -> m (SBatch U n s)
sbComputeP (SBatch arr) = SBatch <$> computeP arr

{-# INLINE sbComputeS #-}
sbComputeS :: Measure' n s
          => SBatch D n s
          -> SBatch U n s
sbComputeS (SBatch arr) = SBatch $ computeS arr


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
(%*), (%+), (%-), (%/) :: (Measure' n s, Source r2 Double, Source r1 Double)
     => SBatch r1 n s
     -> SBatch r2 n s
     -> SBatch D n s
a %* b = sbZipWith (*) a b
a %/ b = sbZipWith (/) a b
a %+ b = sbZipWith (+) a b
a %- b = sbZipWith (-) a b

{-# INLINE sSumAllP #-}
sSumAllP :: (Source r Double, Measure' n s, Monad m)
         => SBatch r n s
         -> m Double
sSumAllP (SBatch a) = sumAllP a

{-# INLINE sSumAllS #-}
sSumAllS :: (Source r Double, Measure' n s)
         => SBatch r n s
         -> Double
sSumAllS (SBatch a) = sumAllS a

-- | Watch out: fromUnboxed, and sbFromUnboxed do not perform length checks.
--   You are advised to use sMapVector
{-# INLINE sbFromUnboxed #-}
sbFromUnboxed :: forall n s.Measure' n s => U.Vector Double -> SBatch U n s
sbFromUnboxed vec = SBatch $ fromUnboxed (mExtent (Proxy :: Proxy (Prepend n s))) vec

{-# INLINE sVectorMap #-}
sVectorMap :: (Measure' n s1, Measure' n s2, Size s1 ~ Size s2)
           => (U.Vector Double -> U.Vector Double)
           -> SBatch U n s1
           -> SBatch U n s2
sVectorMap vf (SBatch arr)
  | U.length vec == U.length vec' = (sbFromUnboxed vec')
  | otherwise                     = error "Vector function did not preserve length"
  where
    vec = toUnboxed arr
    vec' = vf vec

{-# INLINE sReshape #-}
sReshape :: forall r n s1 s2.
          ( Source r Double
          , Size s1 ~ Size s2 -- GHC says this is redundant, GHC is wrong.
          , Measure' n s1
          , Measure' n s2
          )
          => SBatch r n s1
          -> SBatch D n s2
sReshape (SBatch x) = SBatch $ reshape sh x
  where
    sh = mExtent (Proxy :: Proxy (Prepend n s2))

sRandom :: forall s. Measure s => Int -> Double -> Double -> SArray U s
sRandom seed min max = SArray $ R.randomishDoubleArray sh min max seed
  where
    sh = mExtent (Proxy :: Proxy s)

sZeros :: forall s. Measure s => SArray U s
sZeros = SArray . computeS $ fromFunction sh (const 0)
  where
    sh = mExtent (Proxy :: Proxy s)

{-# INLINE batchToArray #-}
batchToArray :: SBatch r n s -> SArray r (Prepend n s)
batchToArray (SBatch arr) = SArray arr

{-# INLINE arrayToBatch #-}
arrayToBatch :: SArray r (Prepend n s) -> SBatch r n s
arrayToBatch (SArray arr) = SBatch arr

{-# INLINE batchMap #-}
batchMap :: (SArray r1 (Prepend n1 s1) -> SArray r2 (Prepend n2 s2)) -> SBatch r1 n1 s1 -> SBatch r2 n2 s2
batchMap f = arrayToBatch . f . batchToArray

corr :: ( oh ~ (ih :- kh :+ 1)
        , ow ~ (iw :- kw :+ 1))
        => SBatch r1 n (ZZ ::. id ::. ih ::. iw)
        -> SArray r2   (ZZ ::. kn ::. kd ::. kh ::. kw)
        -> SBatch D  n (ZZ ::. kn ::. oh ::. ow)

corr = undefined

add :: SBatch r1 n s
    -> SArray r2   s
    -> SBatch D  n s
add = undefined
