{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Core where

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num

data Size = S Nat Nat Nat
data VectorType = Probs | OneHot

type family Prod (s :: Size) :: Nat
type instance Prod ('S a b c) = (a :* b) :* c

