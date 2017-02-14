{-# LANGUAGE ScopedTypeVariables #-}

module Network.Conduits where

import Network
import Conduit
import Static
import Network.Runners
import Control.Applicative

trainC :: forall m i ls.
          ( MonadIO m
          , CreatableNetwork i ls
          )
       => LearningParameters
       -> Conduit ( SArray U i
                  , SArray U (NOutput (Network i ls))
                  ) m (Network i ls)
trainC params = go (randomNetwork 9 :: Network i ls)
  where go net = do mxy <- await
                    case mxy of
                      Just (x, y) -> do (net', (pct, dtl)) <- trainOnce net params x y
                                        liftIO . putStrLn$ show pct ++ '\t':show dtl
                                        yield net'
                                        go net'
                      Nothing -> return ()

combineXY :: Monad m
          => Source m x
          -> Source m y
          -> Source m (x, y)
combineXY sx sy = getZipSource $ liftA2 (,) (ZipSource sx) (ZipSource sy)
