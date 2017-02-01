{-# oOPTIONS_GHC -Odph -rtsopts -threaded -fno-liberate-case -fllvm -optlo-O3
                -funfolding-use-threshold1000 -funfolding-keeness-factor1000 #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
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

newtype SArray r (s :: SMeasure) = SArray (R.Array r (ShapeOf s) Double)
instance Measure s => Show (SArray U s) where
  show (SArray arr) = "Static " <> show arr
instance Measure s => Show (SArray D s) where
  show (SArray arr) = "Delayed Static " <> show (computeS arr :: R.Array U (ShapeOf s ) Double)

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
sFromUnboxed vec = SArray $ fromUnboxed (mExtent (Proxy :: Proxy s)) vec

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

corr :: ( oh ~ (ih :- kh :+ 1)
        , ow ~ (iw :- kw :+ 1))
        => SArray r1 (ZZ ::. id ::. ih ::. iw)
        -> SArray r2 (ZZ ::. kn ::. kd ::. kh ::. kw)
        -> SArray D  (ZZ ::. kn ::. oh ::. ow)

corr = undefined

add :: SArray r1 s
    -> SArray r2 s
    -> SArray D  s
add = undefined
