{-# LANGUAGE DataKinds #-}

import Volume

main :: IO ()
main = print (sRandom 0 0 1 :: SArray U (ZZ))
