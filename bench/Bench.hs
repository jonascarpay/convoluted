{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Criterion
import Criterion.Main

import Static
import Layers
import Network
import Runners
import Data.Functor.Identity

type N = 16

type BaseInput  = (ZZ ::. N ::. 3 ::. 32 ::. 32)
type BaseLayers = '[ Convolution 1 3 9 9 24 24
                   , Pool
                   , ReLU
                   , Convolution 1 1 5 5 8 8
                   , Pool
                   , Flatten
                   , MultiSoftMax '[16]
                   ]

type ConvInput  = (ZZ ::. N ::. 3 ::. 32 ::. 32)
type ConvLayers = '[ Convolution 100 3 32 32 1 1
                   , ReLU
                   , Convolution 10 100 1 1 1 1
                   , Flatten
                   , MultiSoftMax '[10]
                   ]

type FCInput  = (ZZ ::. N ::. 3 ::. 32 ::. 32)
type FCLayers = '[ Flatten
                 , FC 3072 100
                 , ReLU
                 , FC 100 10
                 , MultiSoftMax '[10]
                 ]

type BaseNet = Network BaseInput BaseLayers
type ConvNet = Network ConvInput ConvLayers
type FCNet   = Network FCInput FCLayers

zeroParams = LearningParameters 0 0 0

main :: IO ()
main = defaultMain
  [ bgroup "Base" [ bench "forward"  $ whnf (runIdentity . forward   (randomNetwork 0 :: BaseNet)) sZeros
                  , bench "backward" $ whnf (runIdentity . trainOnce (randomNetwork 0 :: BaseNet) zeroParams sZeros) sZeros
                  ]
  , bgroup "FC"   [ bench "forward"  $ whnf (runIdentity . forward   (randomNetwork 0 :: FCNet)) sZeros
                  , bench "backward" $ whnf (runIdentity . trainOnce (randomNetwork 0 :: FCNet) zeroParams sZeros) sZeros
                  ]
  , bgroup "Conv" [ bench "forward"  $ whnf (runIdentity . forward   (randomNetwork 0 :: ConvNet)) sZeros
                  , bench "backward" $ whnf (runIdentity . trainOnce (randomNetwork 0 :: ConvNet) zeroParams sZeros) sZeros
                  ]
  ]
