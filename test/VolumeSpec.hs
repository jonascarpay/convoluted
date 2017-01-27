{-# LANGUAGE TemplateHaskell #-}

module VolumeSpec where

import Volume
import Data.Vector.Unboxed as U
import Test.QuickCheck

newtype ArbUnboxed = ArbUnboxed (U.Vector Double) deriving (Eq, Show)

instance Arbitrary ArbUnboxed where
  arbitrary = do Positive (Small len) <- arbitrary
                 elems <- vector len
                 return $! ArbUnboxed . fromList $ elems

-- The softMax of a vector has sum one
prop_softMax_sum_one (ArbUnboxed xs) = U.sum (softMax xs) `approx3` 1

approx :: Double -> Double -> Double -> Bool
approx x y epsilon = abs (x - y) < epsilon

approx3 :: Double -> Double -> Bool
x `approx3` y = approx x y 1e-3

return []
runTests = $quickCheckAll
