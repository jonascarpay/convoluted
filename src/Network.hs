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
  { learningRate           :: !Double
  , learningMomentum       :: !Double
  , learningRegularization :: !Double
  } deriving (Eq, Show)

class Updatable l where
  type Gradient l :: *
  applyDelta      :: Monad m => LearningParameters -> l -> Gradient l -> m (l, Gradient l)
  createRandom    :: MonadRandom m => m l

class Updatable l => Layer l (i :: SMeasure) (o :: SMeasure) where
  runForward   :: Monad m => l -> SBatch U n i -> m (SBatch U n o)
  runBackwards :: Monad m => l -> SBatch U n i -> SBatch U n o -> m (Gradient l, SBatch U n o)

data Network (i :: SMeasure) (ls :: [*]) (o :: Nat) where
  NNil  :: Layer x i (ZZ ::. o) => !x                       -> Network i '[x]      o
  NCons :: Layer x i o      => !x -> !(Network o xs no) -> Network i (x ': xs) no

data Gradients :: [*] -> * where
  GNil  :: Updatable x => Gradient x -> Gradients '[x]
  GCons :: Updatable x => Gradient x -> Gradients xs -> Gradients (x ': xs)

class CreatableNetwork (i :: SMeasure) (xs :: [*]) (o :: Nat) where
  randomNetwork :: MonadRandom m => m (Network i xs o)
