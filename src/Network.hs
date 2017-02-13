{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network where

import Static
import Data.Serialize

type Loss = (Double, Double)
data LearningParameters = LearningParameters
  { learningRate           :: Double
  , learningMomentum       :: Double
  , learningRegularization :: Double
  } deriving (Eq, Show)

class Serialize l => Updatable l where
  type Gradient l :: *
  applyDelta      :: Monad m => LearningParameters -> l -> Gradient l -> m l
  randomLayer     :: Int -> l
  zeroLayer       :: l

  randomLayer _    = zeroLayer
  applyDelta _ _ _ = return zeroLayer

-- | An instance of the Layer class is a valid layer for a neural network.
class (Measure i, Updatable l) => Layer (i :: SMeasure) l where
  type LOutput i l :: SMeasure

  runForward   :: (Monad m, Measure i, Measure (LOutput i l))
               => l
               -> SArray U i     -- ^ Input data
               -> m (SArray U (LOutput i l)) -- ^ Output data after passing through this layer

  runBackwards :: (Monad m, Measure i, Measure (LOutput i l))
               => l
               -> SArray U i                 -- ^ Input data during forward pass
               -> SArray U (LOutput i l)                 -- ^ Output data during forward pass. Note that this could be recomputed, but it seems more efficient to keep a reference around.
               -> SArray U (LOutput i l)                 -- ^ Gradient on the output data
               -> m (Gradient l, SArray U i) -- ^ Gradient on the weights, gradient on the input data

class Layer i l => OutputLayer i l where
  runOutput :: Monad m
            => l
            -> SArray U i -- ^ Input data
            -> SArray U (LOutput i l) -- ^ Desired output
            -> m (SArray U i, Loss)

data Network (i :: SMeasure) (ls :: [*])  where
  NNil  :: OutputLayer i l => l                            -> Network i (l ': '[])
  NCons :: Layer i l       => l -> Network (LOutput i l) (ll ': ls) -> Network i (l ': ll ': ls)

class CreatableNetwork (i :: SMeasure) (ls :: [*]) where
  randomNetwork :: Int -> Network i ls
  zeroNetwork   :: Network i ls

instance OutputLayer i l => CreatableNetwork i (l ': '[]) where
  zeroNetwork        = NNil zeroLayer
  randomNetwork seed = NNil (randomLayer seed)

instance ( Layer i l
         , CreatableNetwork (LOutput i l) (ll ': ls)
         ) => CreatableNetwork i (l ': ll ': ls) where

  zeroNetwork        = zeroLayer `NCons` zeroNetwork
  randomNetwork seed = randomLayer seed `NCons` randomNetwork (seed^(9::Int))
