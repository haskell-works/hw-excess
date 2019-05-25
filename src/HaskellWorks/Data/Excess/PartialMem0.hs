module HaskellWorks.Data.Excess.PartialMem0
  ( PartialMem0(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Triplet

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless    as BL
import qualified HaskellWorks.Data.Excess.Internal.Partial.Table as T

class PartialMem0 a where
  partialMem0 :: Int -> a -> Triplet

instance PartialMem0 Word8 where
  partialMem0 n w = Triplet partialMemMin partialMemAll partialMemMax
    where partialMemMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Lo (n * 256 + fromIntegral w)
          partialMemAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Ex (n * 256 + fromIntegral w)
          partialMemMax = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Hi (n * 256 + fromIntegral w)
  {-# INLINE partialMem0 #-}

instance PartialMem0 Word16 where
  partialMem0 n w = Triplet partialMemMin partialMemAll partialMemMax
    where Triplet aMin aAll aMax = partialMem0 8       (fromIntegral  w        :: Word8)
          Triplet bMin bAll bMax = partialMem0 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          partialMemMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMemAll = if n <= 8 then aAll else aAll + bAll
          partialMemMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMem0 #-}

instance PartialMem0 Word32 where
  partialMem0 n w = Triplet partialMemMin partialMemAll partialMemMax
    where Triplet aMin aAll aMax = partialMem0 16       (fromIntegral  w         :: Word16)
          Triplet bMin bAll bMax = partialMem0 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          partialMemMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMemAll = if n <= 8 then aAll else aAll + bAll
          partialMemMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMem0 #-}

instance PartialMem0 Word64 where
  partialMem0 n w = Triplet partialMemMin partialMemAll partialMemMax
    where Triplet aMin aAll aMax = partialMem0 32       (fromIntegral  w         :: Word32)
          Triplet bMin bAll bMax = partialMem0 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          partialMemMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMemAll = if n <= 8 then aAll else aAll + bAll
          partialMemMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMem0 #-}
