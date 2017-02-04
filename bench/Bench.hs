{-# LANGUAGE DataKinds #-}

import Criterion

import Volume
import Layers
import Network

type MyNet = Network '[Convolution 1 3 4 4 2 2, Flatten 1 2 2, MultiSoftMax '[2,2]] (ZZ ::. 4 ::. 4)
myNet = zeroNetwork :: MyNet

myInput = sZeros :: SArray U (NetInput MyNet)

main :: IO ()
main = print (sRandom 0 0 1 :: SArray U (ZZ))
