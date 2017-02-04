{-# LANGUAGE DataKinds #-}

module Layers where

import Network
import Volume
import Layers.MultiSoftMax

myNet :: Network '[MultiSoftMax '[1]] (ZZ ::. 4 ::. 1)
myNet = NNil (MultiSoftMax :: MultiSoftMax '[1])

