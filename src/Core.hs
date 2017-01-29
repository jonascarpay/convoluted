{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Core where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Array.Repa

data Size = S1 Nat
          | S2 Nat Nat
          | S3 Nat Nat Nat
          | S4 Nat Nat Nat Nat

data VectorType = Probs | OneHot

type family Prod (s :: Size) :: Nat
type instance Prod ('S3 a b c) = (a :* b) :* c

newtype Tagged (t :: k) a = Tagged a
