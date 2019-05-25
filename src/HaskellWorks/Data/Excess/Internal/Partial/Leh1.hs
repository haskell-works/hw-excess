module HaskellWorks.Data.Excess.Internal.Partial.Leh1
  ( Leh1(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Triplet

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless    as BL
import qualified HaskellWorks.Data.Excess.Internal.Partial.Table as T

class Leh1 a where
  leh1 :: Int -> a -> Triplet

instance Leh1 Word8 where
  leh1 n w = Triplet lehMin lehAll lehMax
    where lehMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Lo (n * 256 + fromIntegral w)
          lehAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Ex (n * 256 + fromIntegral w)
          lehMax = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Hi (n * 256 + fromIntegral w)
  {-# INLINE leh1 #-}

instance Leh1 Word16 where
  leh1 n w = Triplet lehMin lehAll lehMax
    where Triplet aMin aAll aMax = leh1 8       (fromIntegral  w        :: Word8)
          Triplet bMin bAll bMax = leh1 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          lehMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          lehAll = if n <= 8 then aAll else aAll + bAll
          lehMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE leh1 #-}

instance Leh1 Word32 where
  leh1 n w = Triplet lehMin lehAll lehMax
    where Triplet aMin aAll aMax = leh1 16       (fromIntegral  w         :: Word16)
          Triplet bMin bAll bMax = leh1 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          lehMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          lehAll = if n <= 8 then aAll else aAll + bAll
          lehMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE leh1 #-}

instance Leh1 Word64 where
  leh1 n w = Triplet lehMin lehAll lehMax
    where Triplet aMin aAll aMax = leh1 32       (fromIntegral  w         :: Word32)
          Triplet bMin bAll bMax = leh1 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          lehMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          lehAll = if n <= 8 then aAll else aAll + bAll
          lehMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE leh1 #-}
