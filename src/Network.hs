{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network
  ( module Network
  ) where

import Static
import Util
import Data.Serialize

type Loss = (Double, Double)
data LearningParameters = LearningParameters
  { learningRate           :: Double
  , learningMomentum       :: Double
  , learningRegularization :: Double
  } deriving (Eq, Show)

class ( Creatable l, Serialize l
      ) => Updatable l where
  type Gradient l :: *
  applyDelta      :: Monad m => LearningParameters -> l -> Gradient l -> m l

  applyDelta _ _ _ = return defaultRandom

-- | An instance of the Layer class is a valid layer for a neural network.
class (Measure i, Updatable l, Measure (LOutput i l)) => Layer (i :: SMeasure) l where
  type LOutput i l :: SMeasure

  runForward   :: (Monad m, Measure i)
               => l
               -> SArray U i     -- ^ Input data
               -> m (SArray U (LOutput i l)) -- ^ Output data after passing through this layer

  runBackwards :: (Monad m, Measure i)
               => l
               -> SArray U i                 -- ^ Input data during forward pass
               -> SArray U (LOutput i l)     -- ^ Output data during forward pass.
                                             --   Note that this could be recomputed,
                                             --   but it seems more efficient to keep a reference around.
               -> SArray U (LOutput i l)     -- ^ Gradient on the output data
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

instance OutputLayer i l => Serialize (Network i (l ': '[])) where
  put (NNil l) = put l
  get = do l <- get
           return$ NNil l

instance ( Layer i l
         , Serialize (Network (LOutput i l) (ll ': ls))
         ) => Serialize (Network i (l ': ll ': ls)) where
  put (l `NCons` ls) = do put l
                          put ls

  get = do l <- get
           ls <- get
           return$ l `NCons` ls

type family NOutput n :: SMeasure where
  NOutput (Network i (l ': '[]))     = LOutput i l
  NOutput (Network i (l ': ll ': ls)) = NOutput (Network (LOutput i l) (ll ': ls))

instance OutputLayer i l => Creatable (Network i '[l]) where
  seeded s = NNil$ seeded s

instance ( Layer i l
         , Creatable (Network (LOutput i l) (ll ': ls))
         ) => Creatable (Network i (l ': ll ': ls)) where

  seeded s = seeded s `NCons` seeded s

instance ( OutputLayer i1 l, OutputLayer i2 l
         ) => Cast (Network i1 '[l]) (Network i2 '[l]) where
  cast (NNil l) = NNil l

instance ( Layer i1 l, Layer i2 l
         , Cast (Network (LOutput i1 l) (ll ': ls)) (Network (LOutput i2 l) (ll ': ls))
         ) => Cast (Network i1 (l ': ll ': ls)) (Network i2 (l ': ll ': ls)) where

  cast (l `NCons` ls) = l `NCons` cast ls

