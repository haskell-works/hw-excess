{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Excess.Excess0
    ( Excess0(..)
    ) where

import Data.Word
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1

import qualified Data.Vector.Storable as DVS

class Excess0 v where
  excess0 :: v -> Count -> Int

instance Excess0 Word8 where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}

instance Excess0 Word16 where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}

instance Excess0 Word32 where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}

instance Excess0 Word64 where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}

instance Excess0 (DVS.Vector Word8) where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}

instance Excess0 (DVS.Vector Word16) where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}

instance Excess0 (DVS.Vector Word32) where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}

instance Excess0 (DVS.Vector Word64) where
  excess0 v c = fromIntegral (rank0 v c) - fromIntegral (rank1 v c)
  {-# INLINE excess0 #-}
