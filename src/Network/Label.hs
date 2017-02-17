{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Label
  ( singleton
  , hcat
  , vcat
  , fill
  , maxed
  , fromList
  , (<|>)
  , (<->)
  , toArray
  , fromArray
  , LabelComposite
  , LabelSingle
  ) where

import Static
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.List
import Control.Monad.ST.Strict
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Sequence               as Sq
import qualified Data.Array.Repa as R

type LabelSingle (c :: Nat) = LabelComposite 1 '[c]
newtype LabelComposite (rows :: Nat) (cols :: [Nat]) = Label (Sq.Seq Int)

instance Show (LabelComposite r c) where
  show (Label seq) = "Label (" ++ show seq ++ ")"

instance Eq (LabelComposite r c) where
  (Label s1) == (Label s2) = s1 == s2

singleton :: forall c. KnownNat c => Int -> LabelSingle c
singleton x
  | x >= c = error $ "Label value " ++ show x ++ " too high, max value is " ++ show c
  | x < 0  = error "Label value too low"
  | otherwise = Label . Sq.singleton $ x
  where c = fromInteger$ natVal (Proxy :: Proxy c)

fill :: forall r cs. KnownNat (r :* Length cs) => Int -> LabelComposite r cs
fill x = Label$ Sq.replicate size x
  where size = fromInteger$ natVal (Proxy :: Proxy (r :* Length cs))

-- | Fill every label with its maximum value according to its type
maxed :: forall r cs.
          ( KnownNat r
          , SingI (Concat (Replicate r cs)) )
          => LabelComposite r cs
maxed = Label . Sq.fromList $ vals
  where vals = fromInteger . subtract 1 <$> fromSing (sing :: Sing (Concat (Replicate r cs)))

instance KnownNat c => Num (LabelSingle c) where
  Label (Sq.viewl -> x1 Sq.:< _) + Label (Sq.viewl -> x2 Sq.:< _) = singleton$ x1 + x2
  Label (Sq.viewl -> x1 Sq.:< _) - Label (Sq.viewl -> x2 Sq.:< _) = singleton$ x1 - x2
  Label (Sq.viewl -> x1 Sq.:< _) * Label (Sq.viewl -> x2 Sq.:< _) = singleton$ x1 * x2
  abs (Label (Sq.viewl -> x Sq.:< _)) = singleton$ abs x
  signum = undefined
  fromInteger = singleton . fromInteger

hcat :: LabelComposite 1 c1 -> LabelComposite 1 c2 -> LabelComposite 1 (c1 :++ c2)
hcat (Label s1) (Label s2) = Label$ s1 Sq.>< s2

(<|>) = hcat
(<->) = vcat
infixl 6 <|>
infixl 5 <->

vcat :: LabelComposite r1 cols -> LabelComposite r2 cols -> LabelComposite (r1 :+ r2) cols
vcat (Label s1) (Label s2) = Label$ s1 Sq.>< s2

toArray :: forall r cs. ( KnownNat r
                        , SingI cs
                        , KnownNat (Sum cs)
                        , KnownNat (Sum cs :* r)
                        )
        => LabelComposite r cs
        -> SArray U (ZZ ::. r ::. Sum cs)
toArray (Label seq) = sFromUnboxed vec
  where
    cs = fromSing (sing :: Sing cs)
    size = fromInteger $ natVal (Proxy :: Proxy (Sum cs :* r))

    vec = runST$ do v <- UM.new size
                    writeLabels v 0 (cycle cs) (Sq.viewl seq)
                    U.unsafeFreeze v

    writeLabels _ _      _       Sq.EmptyL    = return ()
    writeLabels v offset (c:cs) ~(n Sq.:< ns) =
      do UM.write v (offset + n) 1
         writeLabels v (offset + fromInteger c) cs (Sq.viewl ns)

fromArray :: forall r cs . SingI cs => SArray U (ZZ ::. r ::. Sum cs) -> LabelComposite r cs
fromArray (SArray (R.toUnboxed -> vec)) = Label$ readLabels vec (cycle cs)
  where
    cs = fromSing (sing :: Sing cs)
    readLabels vec (c:cs) =
      case U.splitAt (fromInteger c) vec of
        (h, t) | U.null h  -> Sq.empty
               | otherwise -> U.maxIndex h Sq.<| readLabels t cs

fromList :: forall r cs. ( KnownNat (r :* Length cs)
            ) => [Int] -> LabelComposite r cs
fromList ns
  | Sq.length seq /= size = error "List length did not match label size"
  | otherwise                  = Label seq
  where seq = Sq.fromList ns
        size = fromInteger$ natVal (Proxy :: Proxy (r :* Length cs))
