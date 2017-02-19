module Creatable (
  Creatable (..),
  defaultRandom
) where

import Data.Serialize

class Serialize a => Creatable a where
  seeded :: Int -> a

defaultRandom :: Creatable a => a
defaultRandom = seeded 0
