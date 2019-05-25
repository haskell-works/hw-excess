module HaskellWorks.Data.Excess.Internal.Partial.Leh0
  ( Leh0(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Triplet

import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless as BL
import qualified HaskellWorks.Data.Excess.Internal.Table.Leh  as T

class Leh0 a where
  leh0 :: Int -> a -> Triplet

instance Leh0 Word8 where
  leh0 n w = Triplet lehMin lehAll lehMax
    where lehMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Lo (n * 256 + fromIntegral w)
          lehAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Ex (n * 256 + fromIntegral w)
          lehMax = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Hi (n * 256 + fromIntegral w)
  {-# INLINE leh0 #-}

instance Leh0 Word16 where
  leh0 n w = Triplet lehMin lehAll lehMax
    where Triplet aMin aAll aMax = leh0 8       (fromIntegral  w        :: Word8)
          Triplet bMin bAll bMax = leh0 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          lehMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          lehAll = if n <= 8 then aAll else aAll + bAll
          lehMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE leh0 #-}

instance Leh0 Word32 where
  leh0 n w = Triplet lehMin lehAll lehMax
    where Triplet aMin aAll aMax = leh0 16       (fromIntegral  w         :: Word16)
          Triplet bMin bAll bMax = leh0 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          lehMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          lehAll = if n <= 8 then aAll else aAll + bAll
          lehMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE leh0 #-}

instance Leh0 Word64 where
  leh0 n w = Triplet lehMin lehAll lehMax
    where Triplet aMin aAll aMax = leh0 32       (fromIntegral  w         :: Word32)
          Triplet bMin bAll bMax = leh0 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          lehMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          lehAll = if n <= 8 then aAll else aAll + bAll
          lehMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE leh0 #-}
