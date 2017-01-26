{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network where

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

class (Prod i ~ Sum cs) => OutputLayer l (i :: Size) (cs :: [Nat]) where
  runOutput :: Monad m => l -> Volume i -> m (Vector cs Probs)
  getError  :: Monad m => l -> Vector cs OneHot -> m (Volume i)

data Size = S Nat Nat Nat

type family Prod (s :: Size) :: Nat
type instance Prod ('S a b c) = (a :* b) :* c

data VectorType = Probs | OneHot

newtype Volume (s :: Size) = Vol Int
newtype Vector (s :: [Nat]) (t :: VectorType) = Vec Int

data Network (i :: Size) (ls :: [*]) (cs :: [Nat]) where
  NNil  :: OutputLayer x i cs => !x                       -> Network i '[x]      cs
  NCons :: Layer x i o        => !x -> !(Network o xs cs) -> Network i (x ': xs) cs

data Gradients :: [*] -> * where
  GNil  :: Updatable x => Gradient x -> Gradients '[x]
  GCons :: Updatable x => Gradient x -> Gradients xs -> Gradients (x ': xs)

class CreatableNetwork (i :: Size) (xs :: [*]) (cs :: [Nat]) where
  randomNetwork :: MonadRandom m => m (Network i xs cs)
