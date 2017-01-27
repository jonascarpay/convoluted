import qualified VolumeSpec as VS

main :: IO ()
main = do _ <- VS.runTests
          return ()
