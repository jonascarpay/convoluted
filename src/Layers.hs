{-# LANGUAGE DataKinds #-}

module Layers where

import Network
import Volume
import Layers.MultiSoftMax

type MyNet = Network '[MultiSoftMax '[2,2]] (ZZ ::. 4 ::. 4)

myNet = zeroNetwork :: MyNet

