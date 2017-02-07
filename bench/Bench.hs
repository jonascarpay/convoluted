{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Criterion
import Criterion.Main
import Criterion.Types

import Volume
import Layers
import Network
import Runners
import Data.Functor.Identity

type NetInput  = (ZZ ::. 16 ::. 3 ::. 32 ::. 32)
type NetOutput = (ZZ ::. 16 ::. 1)

type NetLayers = '[ Convolution 1 3 9 9 24 24
                  , Pool
                  , Convolution 1 1 5 5 8 8
                  , Pool
                  , Pool
                  , Pool
                  , Flatten
                  , MultiSoftMax '[1]
                  ]

type TestNet = Network NetInput NetLayers NetOutput

testNet :: TestNet
testNet = randomNetwork 9

zeroParams = LearningParameters 0 0 0

main :: IO ()
main = defaultMainWith (defaultConfig { timeLimit = 10 })
  [ bench "forward"  $ whnf (runIdentity . forward testNet) sZeros,
    bench "backward" $ whnf (runIdentity . trainOnce testNet zeroParams sZeros) sZeros
  ]
