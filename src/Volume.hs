{-# OPTIONS_GHC -Odph -rtsopts -threaded -fno-liberate-case
                -funfolding-use-threshold1000 -funfolding-keeness-factor1000
                -fllvm -optlo-O3 #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Volume
  ( module Volume
  , module Measure
  , D, U
  )where

import Measure
import Data.Monoid ((<>))
import Data.Singletons.TypeLits
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as U

-- | Volume and Vector hold data that is transferred betweeen layers.
--   For now, these both contain multiple rows of the same data, i.e.
--   are actually a data batch instead of a single sample. These might
--   be promoted to type classes later to accomodate both batches and
--   samples, or different precision data types for working on a GPU.

newtype SArray r            (s :: SMeasure) = SArray (R.Array r (ShapeOf s)        Double)
newtype SBatch r (n :: Nat) (s :: SMeasure) = SBatch (R.Array r (ShapeOf s :. Int) Double)

instance Measure s => Show (SArray D s) where
  show (SArray arr) = "Static " <> show (computeS arr :: R.Array U (ShapeOf s ) Double)

instance Measure s => Show (SBatch D n s) where
  show (SBatch arr) = "Batch "  <> show (computeS arr :: R.Array U (ShapeOf s :. Int) Double)

softMax :: U.Vector Double -> U.Vector Double
softMax !xs = U.map (/expSum) exps
  where xMax   = U.maximum xs
        exps   = U.map (\x -> exp $ x - xMax) xs
        expSum = U.sum exps

-- | Apply softmax to slices of the vector of lengths indicated by the list
multiSoftMax :: [Int] -> U.Vector Double -> U.Vector Double
multiSoftMax !ls !xs = U.concat $ sms (cycle ls) xs
  where
    sms (l:ls) xs | U.null xs = []
                  | U.length xs < l = undefined
                  | otherwise = let (h,t) = U.splitAt l xs
                                 in softMax h : sms ls t

{-# INLINE sFromFunction #-}
sFromFunction :: forall s. Measure s => (ShapeOf s -> Double) -> SArray D s
sFromFunction f = SArray $ fromFunction sh f
  where
    sh = mExtent (proxy :: p s)

{-# INLINE sZipWith #-}
sZipWith :: (Measure s, Source r1 Double, Source r2 Double)
         => (Double -> Double -> Double)
         -> SBatch r1 n s
         -> SBatch r2 n s
         -> SBatch D n s
sZipWith f (SBatch arr1) (SBatch arr2) = SBatch $ R.zipWith f arr1 arr2

{-# INLINE sMap #-}
sMap :: (Source r Double, Measure s)
     => (Double -> Double)
     -> SBatch r n s
     -> SBatch D n s
sMap f (SBatch arr) = SBatch $ R.map f arr

{-# INLINE sComputeP #-}
sComputeP :: (Monad m, Measure s)
          => SBatch D n s     -- TODO: Make polymorphic in representations
          -> m (SBatch U n s) -- TODO: Make polymorphic in representations
sComputeP (SBatch arr) = SBatch <$> computeP arr

{-# INLINE sComputeS #-}
sComputeS :: Measure s
          => SBatch D n s
          -> SBatch U n s
sComputeS (SBatch arr) = SBatch $ computeS arr

{-# INLINE (%*) #-}
{-# INLINE (%+) #-}
{-# INLINE (%-) #-}
{-# INLINE (%/) #-}
(%*), (%+), (%-), (%/) :: (Measure s, Source r2 Double, Source r1 Double)
     => SBatch r1 n s
     -> SBatch r2 n s
     -> SBatch D n s
a %* b = sZipWith (*) a b
a %/ b = sZipWith (/) a b
a %+ b = sZipWith (+) a b
a %- b = sZipWith (-) a b

sSumAllP :: (Source r Double, Measure s, Monad m)
         => SBatch r t s
         -> m Double
sSumAllP (SBatch a) = sumAllP a

sSumAllS :: (Source r Double, Measure s)
         => SBatch r t s
         -> Double
sSumAllS (SBatch a) = sumAllS a
