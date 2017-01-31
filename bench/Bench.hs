{-# LANGUAGE DataKinds #-}

import Volume

main = print (sRandom 0 0 1 :: SArray U (ZZ))
