{-# LANGUAGE DataKinds #-}

import Criterion

import Volume
import Layers
import Network

type NetInput  = (ZZ ::. 1 ::. 3 ::. 32 ::. 32)
type NetOutput = (ZZ ::. 1 ::. 2)
type Layers    = '[ Convolution 2 3 32 32 1 1
                  , Flatten
                  , MultiSoftMax '[2]
                  ]

type MyNet = Network NetInput Layers NetOutput

myNet = zeroNetwork :: MyNet

myInput = sZeros :: SArray U NetInput

main :: IO ()
main = print (sRandom 0 0 1 :: SArray U (ZZ))
