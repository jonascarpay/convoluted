{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network where

import Volume
import Data.Singletons.TypeLits
import Control.Monad.Random

data LearningParameters = LearningParameters
  { learningRate           :: Double
  , learningMomentum       :: Double
  , learningRegularization :: Double
  } deriving (Eq, Show)

class Updatable l where
  type Gradient l :: *
  applyDelta      :: Monad m => LearningParameters -> l -> Gradient l -> m l
  seededRandom    :: Int -> l

-- | An instance of the Layer class is a valid layer for a neural network.
class (Measure o, Updatable l) => Layer l (o :: SMeasure) where

  type InputShape l o :: SMeasure

  runForward   :: (Monad m, Measure (InputShape l o), Measure o)
               => l
               -> SArray U (InputShape l o) -- ^ Input data
               -> m (SArray U o)            -- ^ Output data after passing through this layer

  runBackwards :: (Monad m, Measure (InputShape l o), Measure o)
               => l
               -> SArray U (InputShape l o) -- ^ Input data during forward pass
               -> SArray U o -- ^ Output data during forward pass. Note that this could be recomputed, but it seems more efficient to keep a reference around.
               -> SArray U o -- ^ Gradient on the output data
               -> m (Gradient l, SArray U (InputShape l o), Double) -- ^ Gradient on the weights, gradient on the input data, loss

data Network (i :: SMeasure) (ls :: [*]) (o :: Nat) where
  NNil  :: Layer x (ZZ ::. o) => x ->                    Network (InputShape x (ZZ ::. o)) (x ': '[]) o
  NCons :: Layer x o          => x -> Network o xs no -> Network (InputShape x o)          (x ': xs) no

data Gradients :: [*] -> * where
  GNil  :: Updatable x => (Gradient x)                   -> Gradients (x ': '[])
  GCons :: Updatable x => (Gradient x) -> (Gradients xs) -> Gradients (x ':  xs)

class CreatableNetwork (i :: SMeasure) (xs :: [*]) (o :: Nat) where
  randomNetwork :: MonadRandom m => m (Network i xs o)
