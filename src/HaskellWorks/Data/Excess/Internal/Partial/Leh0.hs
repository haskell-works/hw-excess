module HaskellWorks.Data.Excess.Internal.Partial.Leh0
  ( Leh0(..)
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.Excess.Internal.Table.Leh as T

class Leh0 a where
  leh0Lo :: Int -> a -> Int64
  leh0Ex :: Int -> a -> Int64
  leh0Hi :: Int -> a -> Int64

instance Leh0 Word8 where
  leh0Lo n w = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Lo (n * 256 + fromIntegral w)
  {-# INLINE leh0Lo #-}
  leh0Ex n w = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Ex (n * 256 + fromIntegral w)
  {-# INLINE leh0Ex #-}
  leh0Hi n w = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Hi (n * 256 + fromIntegral w)
  {-# INLINE leh0Hi #-}

instance Leh0 Word16 where
  leh0Lo n w = if n <= 8
    then leh0Lo n (fromIntegral w :: Word8)
    else let  wLo = fromIntegral  w         :: Word8
              wHi = fromIntegral (w .>. 8)  :: Word8
         in min (leh0Lo 8 wLo) (leh0Ex 8 wLo + leh0Lo (n - 8) wHi)
  {-# INLINE leh0Lo #-}
  leh0Ex n w = if n <= 8
    then leh0Ex n (fromIntegral w :: Word8)
    else let  wLo = fromIntegral  w         :: Word8
              wHi = fromIntegral (w .>. 8)  :: Word8
         in leh0Ex 8 wLo + leh0Ex (n - 8) wHi
  {-# INLINE leh0Ex #-}
  leh0Hi n w = if n <= 8
    then leh0Hi n (fromIntegral w :: Word8)
    else let  wLo = fromIntegral  w         :: Word8
              wHi = fromIntegral (w .>. 8)  :: Word8
         in max (leh0Hi 8 wLo) (leh0Ex 8 wLo + leh0Hi (n - 8) wHi)
  {-# INLINE leh0Hi #-}

instance Leh0 Word32 where
  leh0Lo n w = if n <= 16
    then leh0Lo n (fromIntegral w :: Word16)
    else let  wLo = fromIntegral  w         :: Word16
              wHi = fromIntegral (w .>. 16)  :: Word16
          in min (leh0Lo 16 wLo) (leh0Ex 16 wLo + leh0Lo (n - 16) wHi)
  {-# INLINE leh0Lo #-}
  leh0Ex n w = if n <= 16
    then leh0Ex n (fromIntegral w :: Word16)
    else let  wLo = fromIntegral  w         :: Word16
              wHi = fromIntegral (w .>. 16)  :: Word16
          in leh0Ex 16 wLo + leh0Ex (n - 16) wHi
  {-# INLINE leh0Ex #-}
  leh0Hi n w = if n <= 16
    then leh0Hi n (fromIntegral w :: Word16)
    else let  wLo = fromIntegral  w         :: Word16
              wHi = fromIntegral (w .>. 16)  :: Word16
          in max (leh0Hi 16 wLo) (leh0Ex 16 wLo + leh0Hi (n - 16) wHi)
  {-# INLINE leh0Hi #-}

instance Leh0 Word64 where
  leh0Lo n w = if n <= 32
    then leh0Lo n (fromIntegral w :: Word32)
    else let  wLo = fromIntegral  w         :: Word32
              wHi = fromIntegral (w .>. 32)  :: Word32
          in min (leh0Lo 32 wLo) (leh0Ex 32 wLo + leh0Lo (n - 32) wHi)
  {-# INLINE leh0Lo #-}
  leh0Ex n w = if n <= 32
    then leh0Ex n (fromIntegral w :: Word32)
    else let  wLo = fromIntegral  w         :: Word32
              wHi = fromIntegral (w .>. 32)  :: Word32
          in leh0Ex 32 wLo + leh0Ex (n - 32) wHi
  {-# INLINE leh0Ex #-}
  leh0Hi n w = if n <= 32
    then leh0Hi n (fromIntegral w :: Word32)
    else let  wLo = fromIntegral  w         :: Word32
              wHi = fromIntegral (w .>. 32)  :: Word32
          in max (leh0Hi 32 wLo) (leh0Ex 32 wLo + leh0Hi (n - 32) wHi)
  {-# INLINE leh0Hi #-}
