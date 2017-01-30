{-# OPTIONS_GHC -Odph -rtsopts -threaded -fno-liberate-case -fllvm -optlo-O3
                -funfolding-use-threshold1000 -funfolding-keeness-factor1000 #-}


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

newtype SArray r            (s :: SMeasure) = SArray (R.Array r (ShapeOf    s) Double)
newtype SBatch r (n :: Nat) (s :: SMeasure) = SBatch (R.Array r (ShapeOf' n s) Double)

instance Measure s => Show (SArray D s) where
  show (SArray arr) = "Static " <> show (computeS arr :: R.Array U (ShapeOf s ) Double)

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
    sh = mExtent (proxy :: p s)

{-# INLINE sbFromFunction #-}
sbFromFunction :: forall s n. Measure' n s => (ShapeOf' n s -> Double) -> SBatch D n s
sbFromFunction f = SBatch $ fromFunction sh f
  where
    sh = mExtent (proxy :: p (Prepend n s))

{-# INLINE sZipWith #-}
sZipWith :: (Measure' n s, Source r1 Double, Source r2 Double)
         => (Double -> Double -> Double)
         -> SBatch r1 n s
         -> SBatch r2 n s
         -> SBatch D n s
sZipWith f (SBatch arr1) (SBatch arr2) = SBatch $ R.zipWith f arr1 arr2

{-# INLINE sMap #-}
sMap :: (Source r Double, Measure' n s)
     => (Double -> Double)
     -> SBatch r n s
     -> SBatch D n s
sMap f (SBatch arr) = SBatch $ R.map f arr

{-# INLINE sComputeP #-}
sComputeP :: (Monad m, Measure' n s)
          => SBatch D n s     -- TODO: Make polymorphic in representations
          -> m (SBatch U n s) -- TODO: Make polymorphic in representations
sComputeP (SBatch arr) = SBatch <$> computeP arr

{-# INLINE sComputeS #-}
sComputeS :: Measure' n s
          => SBatch D n s
          -> SBatch U n s
sComputeS (SBatch arr) = SBatch $ computeS arr

{-# INLINE (%*) #-}
{-# INLINE (%+) #-}
{-# INLINE (%-) #-}
{-# INLINE (%/) #-}
(%*), (%+), (%-), (%/) :: (Measure' n s, Source r2 Double, Source r1 Double)
     => SBatch r1 n s
     -> SBatch r2 n s
     -> SBatch D n s
a %* b = sZipWith (*) a b
a %/ b = sZipWith (/) a b
a %+ b = sZipWith (+) a b
a %- b = sZipWith (-) a b

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
sbFromUnboxed vec = SBatch $ fromUnboxed (mExtent (proxy :: p (Prepend n s))) vec

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
    sh = mExtent (proxy :: p (Prepend n s2))
