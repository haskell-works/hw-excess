module HaskellWorks.Data.Excess.Internal.Partial.Leh0
  ( Leh0(..)
  ) where

import Data.Int
import Data.Word

import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Excess.Internal.Table as T

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
