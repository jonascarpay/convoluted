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

type NetLayers = '[ Convolution 1 3 9 9 24 24
                  , Pool
                  , ReLU
                  , Convolution 1 1 5 5 8 8
                  , Pool
                  , ReLU
                  , Flatten
                  , MultiSoftMax '[4,4,4,4]
                  ]

type TestNet = Network NetInput NetLayers NetOutput

myNet :: TestNet
myNet = randomNetwork 9

myInput :: SArray U NetInput
myInput = sZeros

main :: IO ()
main = defaultMain [ bench "forward" $ whnf (runIdentity . forward myNet) myInput
                   ]
