{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.Logit where

import Network
import Static
import Data.Serialize

data Logit = Logit

instance Serialize Logit where
  put _ = return ()
  get   = return Logit

instance Creatable Logit where
  seeded _ = Logit

instance Updatable Logit where
  type Gradient Logit = ()
  applyDelta _ _ _ = return Logit

instance Measure s => Layer s Logit where

  type LOutput s Logit = s

  runForward _ x = sComputeP$ sMap logistic x
    where logistic x = 1 / (1 + exp (-x))

  runBackwards _ _ y dy = do dx <- sComputeP$ sZipWith (\y dy -> dy * (y * (1-y))) y dy
                             return ((), dx)
