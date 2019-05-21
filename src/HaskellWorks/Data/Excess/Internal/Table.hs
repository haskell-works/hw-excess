module HaskellWorks.Data.Excess.Internal.Table
  ( genWord8Excess0
  , genWord8Excess1
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Internal.Triplet8 (Triplet8 (Triplet8))

genWord8Excess0 :: Int -> Word8 -> Triplet8
genWord8Excess0 = go 0 0 0
  where go :: Int8 -> Int8 -> Int8 -> Int -> Word8 -> Triplet8
        go lo ex hi n w = if n > 0
          then if w .&. 1 /= 1
            then let fx = ex + 1 in go      lo     fx (max hi fx) (n - 1) (w .>. 1)
            else let fx = ex - 1 in go (min lo fx) fx      hi     (n - 1) (w .>. 1)
          else Triplet8 lo ex hi
{-# INLINE genWord8Excess0 #-}

genWord8Excess1 :: Int -> Word8 -> Triplet8
genWord8Excess1 = go 0 0 0
  where go :: Int8 -> Int8 -> Int8 -> Int -> Word8 -> Triplet8
        go lo ex hi n w = if n > 0
          then if w .&. 1 /= 0
            then let fx = ex + 1 in go      lo     fx (max hi fx) (n - 1) (w .>. 1)
            else let fx = ex - 1 in go (min lo fx) fx      hi     (n - 1) (w .>. 1)
          else Triplet8 lo ex hi
{-# INLINE genWord8Excess1 #-}
