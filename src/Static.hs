{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Static
  ( module Static.Array
  , module Static
  ) where

import Static.Array
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import Data.Array.Repa                      as R
import Data.Array.Repa.Unsafe               as R

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

-- | Batched correlation.
{-# INLINE corrB #-}
corrB :: forall bat ih iw kn kd kh kw oh ow r1 r2.
  ( KnownNat bat, KnownNat kd, KnownNat kh, KnownNat kw
  , Source r1 Double, Source r2 Double
  , Measure (ZZ ::. kn  ::. kd ::. kh ::. kw)
  , Measure (ZZ ::. bat ::. kd ::. ih ::. iw)
  , Measure (ZZ ::. bat ::. kn ::. oh ::. ow)
  , (kh :+ oh :- 1) ~ ih
  , (kw :+ ow :- 1) ~ iw
  )
  => SArray r2 (ZZ ::. kn  ::. kd ::. kh ::. kw)
  -> SArray r1 (ZZ ::. bat ::. kd ::. ih ::. iw)
  -> SArray D  (ZZ ::. bat ::. kn ::. oh ::. ow)
corrB (SArray krns) (SArray imgs) = sFromFunction convF
  where
    kh = fromInteger$ natVal (Proxy :: Proxy kh)
    kw = fromInteger$ natVal (Proxy :: Proxy kw)
    kd = fromInteger$ natVal (Proxy :: Proxy kd)

    convF (Z:.ob:.oz:.oy:.ox) = sumAllS $ krn *^ img
      where
        krn = extract (Z:.oz:.0:.0:.0)   (Z:.1:.kd:.kh:.kw) krns
        img = extract (Z:.ob:.0:.oy:.ox) (Z:.1:.kd:.kh:.kw) imgs

-- | Batched correlation.
{-# INLINE corrVolumesB #-}
corrVolumesB :: forall bat kd kh kw id r1 r2 oh ih ow iw.
                ( KnownNat bat, KnownNat kd, KnownNat id, KnownNat kh, KnownNat kw, KnownNat oh, KnownNat ow, KnownNat ih, KnownNat iw
                , Source r1 Double, Source r2 Double
                , Measure (ZZ ::. bat ::. kd ::. kh ::. kw)
                , Measure (ZZ ::. bat ::. id ::. ih ::. iw)
                , Measure (ZZ ::. kd  ::. id ::. oh ::. ow)
                , (ih ~ (oh :+ kh :- 1)), (iw ~ (ow :+ kw :- 1))
                )
                => SArray r1 (ZZ ::. bat ::. kd ::. kh ::. kw)
                -> SArray r2 (ZZ ::. bat ::. id ::. ih ::. iw)
                -> SArray D  (ZZ ::. kd  ::. id ::. oh ::. ow)
corrVolumesB (SArray krns) (SArray imgs) = sFromFunction convF
  where
    Z:. kb :. _ :. kh :. kw = mExtent (Proxy :: Proxy (ZZ ::. bat ::. kd ::. kh ::. kw))
    convF (Z:.n:.z:.y:.x) = sumAllS $ krn *^ img
      where
        krn = extract (Z :. 0 :. n :. 0 :. 0) (Z :. kb :. 1 :. kh :. kw) krns
        img = extract (Z :. 0 :. z :. y :. x) (Z :. kb :. 1 :. kh :. kw) imgs

type family Halve (n :: Nat) :: Nat where
  Halve 0 = 0
  Halve n = Halve (n :- 2) :+ 1

{-# INLINE sRotateW #-}
sRotateW :: forall r n d h w.
            ( Source r Double , KnownNat n, KnownNat d, KnownNat h, KnownNat w)
         => SArray r (ZZ ::. n ::. d ::. h ::. w)
         -> SArray D (ZZ ::. d ::. n ::. h ::. w)
sRotateW arr = sBackpermute invert arr
  where invert (Z:.n:.z:.y:.x)= Z:.z:.n:.(h-y-1):.(w-x-1)
        h = fromInteger$ natVal (Proxy :: Proxy h)
        w = fromInteger$ natVal (Proxy :: Proxy w)

{-# INLINE sZeropad #-}
sZeropad :: forall b h w h' w' r.
            ( Measure (b ::. h ::. w), Measure (b ::. h' ::. w'), Source r Double
            , KnownNat h, KnownNat h' , KnownNat w, KnownNat w'
            , KnownNat (Halve (w' :- w)), KnownNat (Halve (h' :- h))
            )
         => SArray r (b ::. h  ::. w )
         -> SArray D (b ::. h' ::. w')
sZeropad arr = sTraverse arr padFn
  where
    nw = fromInteger$ natVal (Proxy :: Proxy (Halve (w' :- w)))
    nh = fromInteger$ natVal (Proxy :: Proxy (Halve (h' :- h)))
    h = fromInteger$ natVal (Proxy :: Proxy h)
    w = fromInteger$ natVal (Proxy :: Proxy w)
    padFn lookup (b :. y :. x)
      | y < nh || y >= h + nh || x < nw || x >= w + nw = 0
      | otherwise = lookup (b :. y-nh :. x-nw)

{-# INLINE fullConvB #-}
fullConvB :: forall bat kn kd kh kw ih iw oh ow r1 r2.
             ( KnownNat oh, KnownNat kh, KnownNat ih, KnownNat ow
             , KnownNat kw, KnownNat kn, KnownNat kd, KnownNat iw, KnownNat bat
             , KnownNat (ih :+ 2 :* (kh :- 1))
             , KnownNat (iw :+ 2 :* (kw :- 1))
             , KnownNat (Halve (iw :+ (2 :* (kw :- 1)) :- iw))
             , KnownNat (Halve (ih :+ (2 :* (kh :- 1)) :- ih))
             , Source r1 Double
             , Source r2 Double
             , oh ~ (kh :+ ih :- 1)
             , ow ~ (kw :+ iw :- 1)
             , (kh :+ oh :- 1) ~ (ih :+ 2 :* (kh :- 1) )
             , (kw :+ ow :- 1) ~ (iw :+ 2 :* (kw :- 1) )
             )
          => SArray r1 (ZZ ::. kn  ::. kd ::. kh ::. kw)
          -> SArray r2 (ZZ ::. bat ::. kn ::. ih ::. iw)
          -> SArray D  (ZZ ::. bat ::. kd ::. oh ::. ow)
fullConvB krns imgs = let krn' = sRotateW krns
                          img' = sZeropad imgs
                       in krn' `corrB` img'

{-# INLINE sumOuter' #-}
sumOuter' :: ( KnownNat bat, KnownNat o, Source r Double )
          => SArray r (ZZ ::. bat ::. o)
          -> SArray D (ZZ ::. o)
sumOuter' (SArray arr) = sFromFunction f
  where f (Z:.n) = sumAllS$ unsafeSlice arr (Any:.n)

{-# INLINE sumOuter #-}
sumOuter :: ( Measure (ZZ ::. d2 ::. d3 ::. d4), Source r Double )
         => SArray r (ZZ ::. d1 ::. d2 ::. d3 ::. d4) -> SArray D (ZZ ::. d2 ::. d3 ::. d4)
sumOuter (SArray arr) = sFromFunction (\ (Z:.z:.y:.x) -> sumAllS$ unsafeSlice arr (Any:.z:.y:.x))

{-# INLINE smmMult #-}
smmMult :: ( KnownNat r, KnownNat h, KnownNat c
           , Source r1 Double, Source r2 Double
           )
        => SArray r1 (ZZ ::. r ::. h)
        -> SArray r2 (ZZ ::. h ::. c)
        -> SArray D  (ZZ ::. r ::. c)
smmMult (SArray m1) (SArray m2) = sFromFunction f
  where f (Z:.r:.c) = sumAllS
                    $ R.zipWith (*)
                      (unsafeSlice m1 (Any:.r:.All))
                      (unsafeSlice m2 (Any:.c))

{-# INLINE sTranspose #-}
sTranspose :: ( Source r' Double
              , KnownNat r
              , KnownNat c)
  => SArray r' (ZZ ::. r ::. c)
  -> SArray D  (ZZ ::. c ::. r)
sTranspose arr = sBackpermute (\(Z:.y:.x) -> Z:.x:.y) arr
