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
  runForward   :: Monad m => l -> Volume i -> m (Volume o)
  runBackwards :: Monad m => l -> Volume i -> Volume o -> m (Gradient l, Volume o)

class OutputLayer l (i :: Size) where
  runOutput :: Monad m => l -> Volume i -> m (Vector o Probs)
  getError  :: Monad m => l -> Vector o OneHot -> m (Volume i)

type family Prod (s :: Size) :: Nat
type instance Prod ('S a b c) = (a :* b) :* c

data Network (i :: Size) (ls :: [*]) (o :: Nat) where
  NNil  :: OutputLayer x i => !x                       -> Network i '[x]      (Prod i)
  NCons :: Layer x i o     => !x -> !(Network o xs no) -> Network i (x ': xs) no

data Gradients :: [*] -> * where
  GNil  :: Updatable x => Gradient x -> Gradients '[x]
  GCons :: Updatable x => Gradient x -> Gradients xs -> Gradients (x ': xs)

class CreatableNetwork (i :: Size) (xs :: [*]) (o :: Nat) where
  randomNetwork :: MonadRandom m => m (Network i xs o)
