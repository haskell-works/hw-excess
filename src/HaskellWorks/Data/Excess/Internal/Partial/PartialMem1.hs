module HaskellWorks.Data.Excess.Internal.Partial.PartialMem1
  ( PartialMem1(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Triplet

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless    as BL
import qualified HaskellWorks.Data.Excess.Internal.Partial.Table as T

class PartialMem1 a where
  partialMem1 :: Int -> a -> Triplet

instance PartialMem1 Word8 where
  partialMem1 n w = Triplet partialMemMin partialMemAll partialMemMax
    where partialMemMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Lo (n * 256 + fromIntegral w)
          partialMemAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Ex (n * 256 + fromIntegral w)
          partialMemMax = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Hi (n * 256 + fromIntegral w)
  {-# INLINE partialMem1 #-}

instance PartialMem1 Word16 where
  partialMem1 n w = Triplet partialMemMin partialMemAll partialMemMax
    where Triplet aMin aAll aMax = partialMem1 8       (fromIntegral  w        :: Word8)
          Triplet bMin bAll bMax = partialMem1 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          partialMemMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMemAll = if n <= 8 then aAll else aAll + bAll
          partialMemMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMem1 #-}

instance PartialMem1 Word32 where
  partialMem1 n w = Triplet partialMemMin partialMemAll partialMemMax
    where Triplet aMin aAll aMax = partialMem1 16       (fromIntegral  w         :: Word16)
          Triplet bMin bAll bMax = partialMem1 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          partialMemMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMemAll = if n <= 8 then aAll else aAll + bAll
          partialMemMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMem1 #-}

instance PartialMem1 Word64 where
  partialMem1 n w = Triplet partialMemMin partialMemAll partialMemMax
    where Triplet aMin aAll aMax = partialMem1 32       (fromIntegral  w         :: Word32)
          Triplet bMin bAll bMax = partialMem1 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          partialMemMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMemAll = if n <= 8 then aAll else aAll + bAll
          partialMemMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMem1 #-}
