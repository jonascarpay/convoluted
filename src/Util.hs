{-# LANGUAGE MultiParamTypeClasses #-}

module Util (
  Creatable (..),
  Cast (..),
  defaultRandom
) where

import Data.Serialize

class Serialize a => Creatable a where
  seeded :: Int -> a

defaultRandom :: Creatable a => a
defaultRandom = seeded 0

-- | Cast laws:
--     cast x == cast . cast $ x
class Cast a b where
  cast :: a -> b

