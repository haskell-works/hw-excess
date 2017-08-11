{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Excess.MinMaxExcess1
  ( MinMaxExcess1(..)
  , MaxExcess
  , MinExcess
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.FixedBitSize
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Naive

import qualified Data.Vector.Storable as DVS

type MinExcess = Int
type MaxExcess = Int

class MinMaxExcess1 a where
  minMaxExcess1 :: a -> Triplet

instance MinMaxExcess1 [Bool] where
  minMaxExcess1 = go 0 0 0
    where go minE maxE allE (x:xs)                    = let ne = if x then allE + 1 else allE - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne xs
          go minE maxE allE _                         = Triplet minE allE maxE
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (Naive Word8) where
  minMaxExcess1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (Naive Word16) where
  minMaxExcess1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (Naive Word32) where
  minMaxExcess1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (Naive Word64) where
  minMaxExcess1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 Word8 where
  minMaxExcess1 w = Triplet (word8Excess1Min DVS.! fromIntegral w)
                            (word8Excess1    DVS.! fromIntegral w)
                            (word8Excess1Max DVS.! fromIntegral w)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 Word16 where
  minMaxExcess1 w = Triplet (minExcessA `min` (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (maxExcessA `max` (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = minMaxExcess1 (fromIntegral  w        :: Word8)
          Triplet minExcessB allExcessB maxExcessB = minMaxExcess1 (fromIntegral (w .>. 8) :: Word8)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 Word32 where
  minMaxExcess1 w = Triplet (minExcessA `min` (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (maxExcessA `max` (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = minMaxExcess1 (fromIntegral  w         :: Word16)
          Triplet minExcessB allExcessB maxExcessB = minMaxExcess1 (fromIntegral (w .>. 16) :: Word16)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 Word64 where
  minMaxExcess1 w = Triplet (minExcessA `min` (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (maxExcessA `max` (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = minMaxExcess1 (fromIntegral  w         :: Word32)
          Triplet minExcessB allExcessB maxExcessB = minMaxExcess1 (fromIntegral (w .>. 32) :: Word32)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word8) where
  minMaxExcess1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word8 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess1 w  in
                                            Triplet (minE `min` (wMinE + allE))
                                                    (            wAllE + allE )
                                                    (maxE `max` (wMaxE + allE))
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word16) where
  minMaxExcess1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word16 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess1 w  in
                                            Triplet (minE `min` (wMinE + allE))
                                                    (            wAllE + allE )
                                                    (maxE `max` (wMaxE + allE))
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word32) where
  minMaxExcess1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word32 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess1 w  in
                                            Triplet (minE `min` (wMinE + allE))
                                                    (            wAllE + allE )
                                                    (maxE `max` (wMaxE + allE))
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word64) where
  minMaxExcess1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word64 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = minMaxExcess1 w  in
                                            Triplet (minE `min` (wMinE + allE))
                                                    (            wAllE + allE )
                                                    (maxE `max` (wMaxE + allE))
  {-# INLINE minMaxExcess1 #-}

word8Excess1Min :: DVS.Vector Int
word8Excess1Min =  DVS.fromList
  [ -8, -6, -6, -4, -6, -4, -4, -2, -6, -4, -4, -2, -4, -2, -2,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -7, -5, -5, -3, -5, -3, -3, -1, -5, -3, -3, -1, -3, -1, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  ]

word8Excess1 :: DVS.Vector Int
word8Excess1 =  DVS.fromList
  [ -8, -6, -6, -4, -6, -4, -4, -2, -6, -4, -4, -2, -4, -2, -2,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  2,  2,  4,  2,  4,  4,  6,  2,  4,  4,  6,  4,  6,  6,  8
  ]

word8Excess1Max :: DVS.Vector Int
word8Excess1Max =  DVS.fromList
  [  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  1,  3,  1,  3,  3,  5,  1,  3,  3,  5,  3,  5,  5,  7
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  2,  2,  4,  2,  4,  4,  6,  2,  4,  4,  6,  4,  6,  6,  8
  ]
