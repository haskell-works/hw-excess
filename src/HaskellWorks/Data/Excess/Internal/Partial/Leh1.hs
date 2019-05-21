module HaskellWorks.Data.Excess.Internal.Partial.Leh1
  ( Leh1(..)
  ) where

import Data.Int
import Data.Word

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
