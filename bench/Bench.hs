{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Criterion
import Criterion.Main

import Volume
import Layers
import Network
import Runners
import Data.Functor.Identity

type NetInput  = (ZZ ::. 1 ::. 3 ::. 32 ::. 32)
type NetOutput = (ZZ ::. 1 ::. 16)

type NetLayers = '[ Convolution 1 3 21 21 12 12
                  , Convolution 1 1 5 5 8 8
                  , Pool
                  , Flatten
                  , MultiSoftMax '[16]
                  ]

type TestNet = Network NetInput NetLayers NetOutput

testNet :: TestNet
testNet = randomNetwork 9

zeroParams = LearningParameters 0 0 0

main :: IO ()
main = defaultMain [
  --bench "forward"  $ whnf (runIdentity . forward testNet) sZeros,
  bench "backward" $ whnf (runIdentity . trainOnce testNet zeroParams sZeros) sZeros
                   ]
