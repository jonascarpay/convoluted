{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Volume where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num

data Size = S Nat Nat Nat
data VectorType = Probs | OneHot

newtype Volume (s :: Size) = Vol Int
newtype Vector (s :: Nat) (t :: VectorType) = Vec Int

