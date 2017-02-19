{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Layers.Id where

import Network
import Static
import Data.Serialize

data Id = Id

instance Creatable Id where
  seeded _ = Id

instance Updatable Id where
  type Gradient Id = ()

instance Measure s => Layer s Id where
  type LOutput i Id = i
  runForward _ x = return x
  runBackwards _ _ _ dy = return ((), dy)

instance Measure s => OutputLayer s Id where
  runOutput _ _ y = return (y, error "Id loss")

instance Serialize Id where
  put _ = return ()
  get = return Id
