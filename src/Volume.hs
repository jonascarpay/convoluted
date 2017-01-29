{-# OPTIONS_GHC -Odph -rtsopts -threaded -fno-liberate-case
                -funfolding-use-threshold1000 -funfolding-keeness-factor1000
                -fllvm -optlo-O3 #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Volume
  ( module Volume
  , D, U
  )where

import Core
import Data.List (foldl')
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Array.Repa
import qualified Data.Vector.Unboxed as U

-- | Volume and Vector hold data that is transferred betweeen layers.
--   For now, these both contain multiple rows of the same data, i.e.
--   are actually a data batch instead of a single sample. These might
--   be promoted to type classes later to accomodate both batches and
--   samples, or different precision data types for working on a GPU.

newtype SArray r            (s :: SMeasure) = SArray (Array r (ShapeOf s)      Double)
newtype SBatch r (n :: Nat) (s :: SMeasure) = SBatch (Array r (ShapeOf s:.Int) Double)

instance Show (SBatch r n s) where
  show _ = "aaa"

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
