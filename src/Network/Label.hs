{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Label where

import Static
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.List
import Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable as U
import Control.Monad.ST.Strict

newtype Label (c :: Nat) = Label Int

data LabelSeq (cs :: [Nat]) where
  LNil  :: LabelSeq '[]
  LCons :: KnownNat c => Label c -> LabelSeq cs -> LabelSeq (c ': cs)

data LabelBatch (n :: Nat) (cs :: [Nat]) where
  LBNil :: LabelBatch 0 cs
  LBCons :: KnownNat n => LabelSeq cs -> LabelBatch n cs -> LabelBatch (n :+ 1) cs

toSArray :: forall cs n.
  ( KnownNat n
  , KnownNat (Sum cs)
  , KnownNat (Size (ZZ ::. n ::. Sum cs))
  , SingI cs
  )
  => LabelBatch n cs
  -> SArray U (ZZ ::. n ::. Sum cs)
toSArray = sFromUnboxed . toVector

class ToVector l where
  toVector :: l -> Vector Double

instance KnownNat c => ToVector (Label c) where
  toVector (Label n) = runST$ do let len = fromInteger$ natVal (Proxy :: Proxy c)
                                 vec <- new len
                                 write vec n 1
                                 unsafeFreeze vec

instance ToVector (LabelSeq cs) where
  toVector ls = U.concat $ go ls
    where
      go :: LabelSeq cs' -> [Vector Double]
      go LNil = []
      go (l `LCons` ls) = toVector l : go ls

instance ToVector (LabelBatch n cs) where
  toVector lbs = U.concat $ go lbs
    where
      go :: LabelBatch n' cs' -> [Vector Double]
      go LBNil = []
      go (lb `LBCons` lbs) = toVector lb : go lbs
