{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Excess.Mem0
  ( Mem0(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.FixedBitSize
import HaskellWorks.Data.Excess.Internal.Table
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Naive

import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless as BL

class Mem0 a where
  mem0 :: a -> Triplet

instance Mem0 [Bool] where
  mem0 = go 0 0 0
    where go minE maxE allE (x:xs)                    = let ne = if x then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne xs
          go minE maxE allE _                         = Triplet minE allE maxE
  {-# INLINE mem0 #-}

instance Mem0 (Naive Word8) where
  mem0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem0 #-}

instance Mem0 (Naive Word16) where
  mem0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem0 #-}

instance Mem0 (Naive Word32) where
  mem0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem0 #-}

instance Mem0 (Naive Word64) where
  mem0 = go 0 0 0 0 . naive
    where go minE maxE allE n w | n < fixedBitSize w  = let ne = if w .?. fromIntegral n then allE - 1 else allE + 1 in
                                                        go (BL.minInt minE ne) (BL.maxInt maxE ne) ne (n + 1) w
          go minE maxE allE _ _                       = Triplet minE allE maxE
  {-# INLINE mem0 #-}

instance Mem0 Word8 where
  mem0 w = Triplet (DVS.unsafeIndex word8Excess0Min (fromIntegral w))
                            (DVS.unsafeIndex word8Excess0    (fromIntegral w))
                            (DVS.unsafeIndex word8Excess0Max (fromIntegral w))
  {-# INLINE mem0 #-}

instance Mem0 Word16 where
  mem0 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = mem0 (fromIntegral  w        :: Word8)
          Triplet minExcessB allExcessB maxExcessB = mem0 (fromIntegral (w .>. 8) :: Word8)
  {-# INLINE mem0 #-}

instance Mem0 Word32 where
  mem0 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = mem0 (fromIntegral  w         :: Word16)
          Triplet minExcessB allExcessB maxExcessB = mem0 (fromIntegral (w .>. 16) :: Word16)
  {-# INLINE mem0 #-}

instance Mem0 Word64 where
  mem0 w = Triplet (BL.minInt minExcessA (minExcessB + allExcessA))
                            (allExcessA + allExcessB)
                            (BL.maxInt maxExcessA (maxExcessB + allExcessA))
    where Triplet minExcessA allExcessA maxExcessA = mem0 (fromIntegral  w         :: Word32)
          Triplet minExcessB allExcessB maxExcessB = mem0 (fromIntegral (w .>. 32) :: Word32)
  {-# INLINE mem0 #-}

instance Mem0 (DVS.Vector Word8) where
  mem0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word8 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem0 #-}

instance Mem0 (DVS.Vector Word16) where
  mem0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word16 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem0 #-}

instance Mem0 (DVS.Vector Word32) where
  mem0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word32 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem0 #-}

instance Mem0 (DVS.Vector Word64) where
  mem0 = DVS.foldl' gen (Triplet 0 0 0)
    where gen :: Triplet -> Word64 -> Triplet
          gen (Triplet minE allE maxE) w  = let Triplet wMinE wAllE wMaxE = mem0 w  in
                                            Triplet (BL.minInt minE (wMinE + allE))
                                                    (                wAllE + allE )
                                                    (BL.maxInt maxE (wMaxE + allE))
  {-# INLINE mem0 #-}
