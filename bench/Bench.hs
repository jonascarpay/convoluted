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
type NetOutput = (ZZ ::. 1 ::. 2)
type NetLayers = '[ Convolution 2 3 32 32 1 1
                  , Flatten
                  , MultiSoftMax '[2]
                  ]

type TestNet = Network NetInput NetLayers NetOutput

myNet :: TestNet
myNet = randomNetwork 9

myInput :: SArray U NetInput
myInput = sZeros

main :: IO ()
main = defaultMain [ bench "forward" $ whnf (runIdentity . forward myNet) myInput
                   ]
