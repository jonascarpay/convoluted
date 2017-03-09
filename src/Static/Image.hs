{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Static.Image
  ( readRaw
  , extract
  , saveImg
  , saveMany
  , sHcat
  , BMP
  ) where

import Static
import Util
import Data.Array.Repa hiding (extract, (++))
import Data.Array.Repa.IO.BMP
import Data.Word
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num ((:*))
import Data.Proxy
import Control.Monad

type BMP = Array U DIM2 (Word8, Word8, Word8)

readRaw :: FilePath -> IO (Either String BMP)
readRaw fp = do ebmp <- readImageFromBMP fp
                return $ case ebmp of
                   Left err -> Left $! show err
                   Right x  -> Right x

saveMany :: (KnownNat n, KnownNat h, KnownNat w) => FilePath -> SArray U (ZZ ::. n ::. 3 ::. h ::. w) -> IO ()
saveMany p imgs = do imgs <- split imgs
                     zipWithM_ saveImg ((p++) . show <$> [(0::Int)..]) imgs

split :: forall m n d h w. (KnownNat n, Monad m, KnownNat d, KnownNat h, KnownNat w)
      => SArray U (ZZ ::. n ::. d ::. h ::. w) -> m [SArray U (ZZ ::. d ::. h ::. w)]
split arr = sequence$ slice' arr <$> [0..n-1]
  where n = fromInteger$ natVal (Proxy :: Proxy n)
        slice' :: SArray U (ZZ ::. n ::. d ::. h ::. w) -> Int -> m (SArray U (ZZ ::. d ::. h ::. w))
        slice' (SArray arr) n = sComputeP$ sFromFunction (\ (Z:.z:.y:.x) -> arr ! ix4 n z y x)

saveImg :: forall h w. (KnownNat h, KnownNat w) => FilePath -> SArray U (ZZ ::. 3 ::. h ::. w) -> IO ()
saveImg p (SArray arr) = computeP img >>= writeImageToBMP (p ++ ".bmp")
  where scale x = round . (*255) $ (x - min') / (max' - min')
        min' = foldAllS min (1/0)  arr
        max' = foldAllS max (-1/0) arr
        pxfn (Z:.y:.x) = ( scale$ arr ! ix3 0 (h-y-1) x
                         , scale$ arr ! ix3 1 (h-y-1) x
                         , scale$ arr ! ix3 2 (h-y-1) x)
        h = fromInteger$ natVal (Proxy :: Proxy h)
        w = fromInteger$ natVal (Proxy :: Proxy w)
        img = fromFunction (Z :. h :. w) pxfn

sHcat :: forall m n d h w. ( Monad m
         , KnownNat n, KnownNat d, KnownNat h, KnownNat w, KnownNat (n :* w)
                           ) => SArray U (ZZ ::. n ::. d ::. h ::. w) -> m (SArray U (ZZ ::. d ::. h ::. n :* w))

sHcat arr = sComputeP$ sTraverse arr f
  where w = fromInteger$ natVal (Proxy :: Proxy w)
        f lk (Z :. z :. y :. x) = let (d,m) = x `divMod` w
                                   in lk (ix4 d z y m)

extract :: forall h w. (KnownNat h, KnownNat w)
        => BMP -> Rect Double -> SArray D (ZZ ::. 3 ::. h ::. w)
extract img (Rect cx cy cw ch)
  | invalid   = error "Crop rectangle out of bounds"
  | otherwise = sFromFunction fn
  where

    invalid = cx < 0 || cy < 0 || cx + cw > 1 || cy + ch > 1

    Z :. ih' :. iw' = extent img
    iw = fromIntegral iw'
    ih = fromIntegral ih'
    oh = fromInteger$ natVal (Proxy :: Proxy h)
    ow = fromInteger$ natVal (Proxy :: Proxy w)

    px x = round$ iw * (cx + fromIntegral x * cw / ow)
    py y = round$ ih * (cy + fromIntegral y * ch / oh)

    fn (Z :. 0 :. y :. x) = let (r,_,_) = img ! (Z :. ih' - py y :. px x) in fromIntegral r / 255 - 0.5
    fn (Z :. 1 :. y :. x) = let (_,g,_) = img ! (Z :. ih' - py y :. px x) in fromIntegral g / 255 - 0.5
    fn (Z :. 2 :. y :. x) = let (_,_,b) = img ! (Z :. ih' - py y :. px x) in fromIntegral b / 255 - 0.5
