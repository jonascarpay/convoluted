{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.ReLU where

import Network
import Static
import Data.Serialize

data ReLU = ReLU

instance Updatable ReLU where
  type Gradient ReLU = ReLU
  zeroLayer = ReLU

instance Measure s => Layer s ReLU where
  type LOutput i ReLU = i
  runForward _ x = sComputeP$ sMap (max 0) x
  runBackwards _ _ y dy =
    do dx <- sComputeP$ sZipWith (\x t -> if t > 0 then x else 0) dy y
       return (ReLU, dx)

instance Serialize ReLU where
  put _ = return ()
  get = return ReLU
