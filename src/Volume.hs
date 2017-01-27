{-# OPTIONS_GHC -Odph -rtsopts -threaded -fno-liberate-case
                -funfolding-use-threshold1000 -funfolding-keeness-factor1000
                -fllvm -optlo-O3 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Volume where

import Core
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Array.Repa
import Data.Vector.Unboxed as U

-- | Volume and Vector hold data that is transferred betweeen layers.
--   For now, these both contain multiple rows of the same data, i.e.
--   are actually a data batch instead of a single sample. These might
--   be promoted to type classes later to accomodate both batches and
--   samples, or different precision data types for working on a GPU.

newtype Volume  (s :: Size)                  = Vol (Array U DIM4 Double)
newtype Weights (s :: Size)                  = Wts (Array U DIM4 Double)
newtype Vector  (s :: Nat) (t :: VectorType) = Vec (Array U DIM2 Double)

softMax :: U.Vector Double -> U.Vector Double
softMax xs = U.map (/expSum) exps
  where xMax   = U.maximum xs
        exps   = U.map (\x -> exp $ x - xMax) xs
        expSum = U.sum exps
