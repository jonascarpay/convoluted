{-# LANGUAGE TemplateHaskell #-}

module VolumeSpec where

import Volume
import Data.Vector.Unboxed as U
import Test.QuickCheck

newtype ArbUnboxed = ArbUnboxed (U.Vector Double) deriving (Eq, Show)

instance Arbitrary ArbUnboxed where
  arbitrary = do Positive len <- arbitrary
                 elems <- vector len
                 return $! ArbUnboxed . fromList $ elems

-- The softMax of a vector has sum one
prop_softMax_sum_one          (ArbUnboxed xs) = U.sum (softMax xs) `approx3` 1
-- applying softMax does not change the vector length
prop_softMax_length_invariant (ArbUnboxed xs) = U.length xs == U.length (softMax xs)

-- If the segment length is equal to the vector length, multiSoftMax
-- is equivalent to regular softMax
prop_multiSoftMax_single           (ArbUnboxed xs) =
  softMax xs == multiSoftMax [U.length xs] xs
prop_multiSoftMax_length_invariant (ArbUnboxed xs) =
  U.length xs == U.length (multiSoftMax [1] xs)


approx :: Double -> Double -> Double -> Bool
approx x y epsilon = abs (x - y) < epsilon

approx3 :: Double -> Double -> Bool
x `approx3` y = approx x y 1e-3

return []
runTests = $quickCheckAll
