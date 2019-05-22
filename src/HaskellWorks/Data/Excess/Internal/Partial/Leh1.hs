module HaskellWorks.Data.Excess.Internal.Partial.Leh1
  ( Leh1(..)
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Excess.Internal.Table as T

class Leh1 a where
  leh1Lo :: Int -> a -> Int64
  leh1Ex :: Int -> a -> Int64
  leh1Hi :: Int -> a -> Int64

instance Leh1 Word8 where
  leh1Lo n w = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Lo (n * 256 + fromIntegral w)
  {-# INLINE leh1Lo #-}
  leh1Ex n w = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Ex (n * 256 + fromIntegral w)
  {-# INLINE leh1Ex #-}
  leh1Hi n w = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Hi (n * 256 + fromIntegral w)
  {-# INLINE leh1Hi #-}

instance Leh1 Word16 where
  leh1Lo n w = if n <= 8
    then leh1Lo n (fromIntegral w :: Word8)
    else let  wLo = fromIntegral  w         :: Word8
              wHi = fromIntegral (w .>. 8)  :: Word8
          in min (leh1Lo 8 wLo) (leh1Ex 8 wLo + leh1Lo (n - 8) wHi)
  {-# INLINE leh1Lo #-}
  leh1Ex n w = if n <= 8
    then leh1Ex n (fromIntegral w :: Word8)
    else let  wLo = fromIntegral  w         :: Word8
              wHi = fromIntegral (w .>. 8)  :: Word8
          in leh1Ex 8 wLo + leh1Ex (n - 8) wHi
  {-# INLINE leh1Ex #-}
  leh1Hi n w = if n <= 8
    then leh1Hi n (fromIntegral w :: Word8)
    else let  wLo = fromIntegral  w         :: Word8
              wHi = fromIntegral (w .>. 8)  :: Word8
          in max (leh1Hi 8 wLo) (leh1Ex 8 wLo + leh1Hi (n - 8) wHi)
  {-# INLINE leh1Hi #-}

instance Leh1 Word32 where
  leh1Lo n w = if n <= 16
    then leh1Lo n (fromIntegral w :: Word16)
    else let  wLo = fromIntegral  w         :: Word16
              wHi = fromIntegral (w .>. 16) :: Word16
          in min (leh1Lo 16 wLo) (leh1Ex 16 wLo + leh1Lo (n - 16) wHi)
  {-# INLINE leh1Lo #-}
  leh1Ex n w = if n <= 16
    then leh1Ex n (fromIntegral w :: Word16)
    else let  wLo = fromIntegral  w         :: Word16
              wHi = fromIntegral (w .>. 16) :: Word16
          in leh1Ex 16 wLo + leh1Ex (n - 16) wHi
  {-# INLINE leh1Ex #-}
  leh1Hi n w = if n <= 16
    then leh1Hi n (fromIntegral w :: Word16)
    else let  wLo = fromIntegral  w         :: Word16
              wHi = fromIntegral (w .>. 16) :: Word16
          in max (leh1Hi 16 wLo) (leh1Ex 16 wLo + leh1Hi (n - 16) wHi)
  {-# INLINE leh1Hi #-}

instance Leh1 Word64 where
  leh1Lo n w = if n <= 32
    then leh1Lo n (fromIntegral w :: Word32)
    else let  wLo = fromIntegral  w         :: Word32
              wHi = fromIntegral (w .>. 32) :: Word32
          in min (leh1Lo 32 wLo) (leh1Ex 32 wLo + leh1Lo (n - 32) wHi)
  {-# INLINE leh1Lo #-}
  leh1Ex n w = if n <= 32
    then leh1Ex n (fromIntegral w :: Word32)
    else let  wLo = fromIntegral  w         :: Word32
              wHi = fromIntegral (w .>. 32) :: Word32
          in leh1Ex 32 wLo + leh1Ex (n - 32) wHi
  {-# INLINE leh1Ex #-}
  leh1Hi n w = if n <= 32
    then leh1Hi n (fromIntegral w :: Word32)
    else let  wLo = fromIntegral  w         :: Word32
              wHi = fromIntegral (w .>. 32) :: Word32
          in max (leh1Hi 32 wLo) (leh1Ex 32 wLo + leh1Hi (n - 32) wHi)
  {-# INLINE leh1Hi #-}
