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

data LearningParameters = LearningParameters
  { learningRate           :: Double
  , learningMomentum       :: Double
  , learningRegularization :: Double
  } deriving (Eq, Show)

class Updatable l where
  type Gradient l :: *
  applyDelta      :: Monad m => LearningParameters -> l -> Gradient l -> m l
  randomLayer     :: Int -> l
  zeroLayer       :: l

  randomLayer _    = zeroLayer
  applyDelta _ _ _ = return zeroLayer

-- | An instance of the Layer class is a valid layer for a neural network.
class (Measure i, Measure o, Updatable l) => Layer (i :: SMeasure) l (o :: SMeasure) | i l -> o where

  runForward   :: (Monad m, Measure i, Measure o)
               => l
               -> SArray U i     -- ^ Input data
               -> m (SArray U o) -- ^ Output data after passing through this layer

  runBackwards :: (Monad m, Measure i, Measure o)
               => l
               -> SArray U i                 -- ^ Input data during forward pass
               -> SArray U o                 -- ^ Output data during forward pass. Note that this could be recomputed, but it seems more efficient to keep a reference around.
               -> SArray U o                 -- ^ Gradient on the output data
               -> m (Gradient l, SArray U i) -- ^ Gradient on the weights, gradient on the input data

class Layer i l o => OutputLayer i l o where
  runOutput :: Monad m
            => l
            -> SArray U i -- ^ Input data
            -> SArray U o -- ^ Desired output
            -> m (SArray U i, Double)

data Network (i :: SMeasure) (ls :: [*]) (o :: SMeasure) where
  NNil  :: OutputLayer i l o => l                            -> Network i (l ': '[])      o
  NCons :: Layer i l o       => l -> Network o (ll ': ls) o' -> Network i (l ': ll ': ls) o'

class CreatableNetwork (i :: SMeasure) (ls :: [*]) (o :: SMeasure) where
  randomNetwork :: Int -> Network i ls o
  zeroNetwork   :: Network i ls o

instance OutputLayer i l o => CreatableNetwork i (l ': '[]) o where
  zeroNetwork        = NNil zeroLayer
  randomNetwork seed = NNil (randomLayer seed)

instance ( Layer i l o
         , CreatableNetwork o (ll ': ls) o'
         ) => CreatableNetwork i (l ': ll ': ls) o' where

  zeroNetwork        = zeroLayer `NCons` zeroNetwork
  randomNetwork seed = randomLayer seed `NCons` randomNetwork (seed^(9::Int))
