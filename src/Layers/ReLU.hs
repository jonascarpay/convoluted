{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.ReLU where

import Network
import Volume
import Data.Array.Repa

data ReLU = ReLU

instance Updatable ReLU where
  type Gradient ReLU = ReLU
  zeroLayer = ReLU

instance Measure s => Layer s ReLU s where
  runForward _ x = sComputeP$ sMap (max 0) x
  runBackwards _ (SArray x) _ (SArray dy) =
    do dx <- sComputeP$ sFromFunction f
       return (ReLU, dx)
     where
       f pos
         | x ! pos > 0 = dy ! pos
         | otherwise   = 0
