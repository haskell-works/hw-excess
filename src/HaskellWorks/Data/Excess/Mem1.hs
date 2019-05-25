{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Excess.Mem1
  ( Mem1(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.FixedBitSize
import HaskellWorks.Data.Excess.Internal.Table
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Naive

import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless as BL

class Mem1 a where
  mem1 :: a -> Triplet

instance Mem1 [Bool] where
  mem1 = go 0 0 0
    where go minE maxE allE (x:xs)                    = let ne = if x then allE + 1 else allE - 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne xs
          go minE maxE allE _                         = Triplet minE allE maxE
  {-# INLINE mem1 #-}

instance Mem1 (Naive Word8) where
  mem1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem1 #-}

instance Mem1 (Naive Word16) where
  mem1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem1 #-}

instance Mem1 (Naive Word32) where
  mem1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem1 #-}

instance Mem1 (Naive Word64) where
  mem1 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE + 1 else allE - 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem1 #-}

instance Mem1 Word8 where
  mem1 w = Triplet (DVS.unsafeIndex word8Excess1Min  (fromIntegral w))
                            (DVS.unsafeIndex word8Excess1     (fromIntegral w))
                            (DVS.unsafeIndex word8Excess1Max  (fromIntegral w))
  {-# INLINE mem1 #-}

instance Mem1 Word16 where
  mem1 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = mem1 (fromIntegral  w        :: Word8)
          Triplet minExcessB allExcessB maxExcessB = mem1 (fromIntegral (w .>. 8) :: Word8)
  {-# INLINE mem1 #-}

instance Mem1 Word32 where
  mem1 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = mem1 (fromIntegral  w         :: Word16)
          Triplet minExcessB allExcessB maxExcessB = mem1 (fromIntegral (w .>. 16) :: Word16)
  {-# INLINE mem1 #-}

instance Mem1 Word64 where
  mem1 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = mem1 (fromIntegral  w         :: Word32)
          Triplet minExcessB allExcessB maxExcessB = mem1 (fromIntegral (w .>. 32) :: Word32)
  {-# INLINE mem1 #-}

instance Mem1 (DVS.Vector Word8) where
  mem1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word8 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem1 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem1 #-}

instance Mem1 (DVS.Vector Word16) where
  mem1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word16 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem1 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem1 #-}

instance Mem1 (DVS.Vector Word32) where
  mem1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word32 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem1 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem1 #-}

instance Mem1 (DVS.Vector Word64) where
  mem1 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word64 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem1 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem1 #-}
