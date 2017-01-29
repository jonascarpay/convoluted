{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network where

import Core
import Volume
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.List
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

class Updatable l => Layer l (i :: Size) (o :: Size) where
  runForward   :: Monad m => l -> SBatch U i -> m (SBatch U o)
  runBackwards :: Monad m => l -> SBatch U i -> SBatch U o -> m (Gradient l, SBatch U o)

data Network (i :: Size) (ls :: [*]) (o :: Nat) where
  NNil  :: Layer x i (S1 o) => !x                       -> Network i '[x]      o
  NCons :: Layer x i o      => !x -> !(Network o xs no) -> Network i (x ': xs) no

data Gradients :: [*] -> * where
  GNil  :: Updatable x => Gradient x -> Gradients '[x]
  GCons :: Updatable x => Gradient x -> Gradients xs -> Gradients (x ': xs)

class CreatableNetwork (i :: Size) (xs :: [*]) (o :: Nat) where
  randomNetwork :: MonadRandom m => m (Network i xs o)
