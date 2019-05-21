{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Excess.MinMaxExcess0Spec (spec) where

import HaskellWorks.Data.Bits.Word
import HaskellWorks.Data.Excess.Internal.Table
import HaskellWorks.Data.Excess.Internal.Triplet8 (Triplet8 (Triplet8))
import HaskellWorks.Data.Excess.MinMaxExcess0
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Naive
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable as DVS
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Excess.MinMaxExcess0Spec" $ do
  describe "For Word8" $ do
    it "Excess should be between min excess and max excess" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      let Triplet minE e maxE = minMaxExcess0 w
      assert $ minE <= e && e <= maxE
    it "minE2 == minE0 `min` (minE1 + e0)" $ requireProperty $ do
      w0 <- forAll $ G.word8 R.constantBounded
      w1 <- forAll $ G.word8 R.constantBounded
      let w2 = leConcat w0 w1
      let Triplet minE0 e0 _ = minMaxExcess0 w0
      let Triplet minE1 _  _ = minMaxExcess0 w1
      let Triplet minE2 _  _ = minMaxExcess0 w2
      minE2 === minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0)" $ requireProperty $ do
      w0 <- forAll $ G.word8 R.constantBounded
      w1 <- forAll $ G.word8 R.constantBounded
      let w2 = leConcat w0 w1
      let Triplet _ e0 maxE0 = minMaxExcess0 w0
      let Triplet _ _  maxE1 = minMaxExcess0 w1
      let Triplet _ _  maxE2 = minMaxExcess0 w2
      maxE2 === maxE0 `max` (maxE1 + e0)
    it "minE2 == minE0 `min` (minE1 + e0) via vector" $ requireProperty $ do
      w0 <- forAll $ G.word8 R.constantBounded
      w1 <- forAll $ G.word8 R.constantBounded
      let w2 = DVS.fromList [w0, w1]
      let Triplet minE0 e0 _ = minMaxExcess0 w0
      let Triplet minE1 _  _ = minMaxExcess0 w1
      let Triplet minE2 _  _ = minMaxExcess0 w2
      minE2 === minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $ requireProperty $ do
      w0 <- forAll $ G.word8 R.constantBounded
      w1 <- forAll $ G.word8 R.constantBounded
      let w2 = DVS.fromList [w0, w1]
      let Triplet _ e0 maxE0 = minMaxExcess0 w0
      let Triplet _ _  maxE1 = minMaxExcess0 w1
      let Triplet _ _  maxE2 = minMaxExcess0 w2
      maxE2 === maxE0 `max` (maxE1 + e0)
  describe "For Word16" $ do
    it "Excess should be between min excess and max excess" $ requireProperty $ do
      w <- forAll $ G.word16 R.constantBounded
      let Triplet minE e maxE = minMaxExcess0 w
      assert $ minE <= e && e <= maxE
    it "minE2 == minE0 `min` (minE1 + e0)" $ requireProperty $ do
      w0 <- forAll $ G.word16 R.constantBounded
      w1 <- forAll $ G.word16 R.constantBounded
      let w2 = leConcat w0 w1
      let Triplet minE0 e0 _ = minMaxExcess0 w0
      let Triplet minE1 _  _ = minMaxExcess0 w1
      let Triplet minE2 _  _ = minMaxExcess0 w2
      minE2 === minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0)" $ requireProperty $ do
      w0 <- forAll $ G.word16 R.constantBounded
      w1 <- forAll $ G.word16 R.constantBounded
      let w2 = leConcat w0 w1
      let Triplet _ e0 maxE0 = minMaxExcess0 w0
      let Triplet _ _  maxE1 = minMaxExcess0 w1
      let Triplet _ _  maxE2 = minMaxExcess0 w2
      maxE2 === maxE0 `max` (maxE1 + e0)
    it "minE2 == minE0 `min` (minE1 + e0) via vector" $ requireProperty $ do
      w0 <- forAll $ G.word16 R.constantBounded
      w1 <- forAll $ G.word16 R.constantBounded
      let w2 = DVS.fromList [w0, w1]
      let Triplet minE0 e0 _ = minMaxExcess0 w0
      let Triplet minE1 _  _ = minMaxExcess0 w1
      let Triplet minE2 _  _ = minMaxExcess0 w2
      minE2 === minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $ requireProperty $ do
      w0 <- forAll $ G.word16 R.constantBounded
      w1 <- forAll $ G.word16 R.constantBounded
      let w2 = DVS.fromList [w0, w1]
      let Triplet _ e0 maxE0 = minMaxExcess0 w0
      let Triplet _ _  maxE1 = minMaxExcess0 w1
      let Triplet _ _  maxE2 = minMaxExcess0 w2
      maxE2 === maxE0 `max` (maxE1 + e0)
    describe "For Word32" $ do
      it "Excess should be between min excess and max excess" $ requireProperty $ do
        w <- forAll $ G.word32 R.constantBounded
        let Triplet minE e maxE = minMaxExcess0 w
        assert $ minE <= e && e <= maxE
      it "minE2 == minE0 `min` (minE1 + e0)" $ requireProperty $ do
        w0 <- forAll $ G.word32 R.constantBounded
        w1 <- forAll $ G.word32 R.constantBounded

        let w2 = leConcat w0 w1
        let Triplet minE0 e0 _ = minMaxExcess0 w0
        let Triplet minE1 _  _ = minMaxExcess0 w1
        let Triplet minE2 _  _ = minMaxExcess0 w2
        minE2 === minE0 `min` (minE1 + e0)
      it "maxE2 == maxE0 `max` (maxE1 + e0)" $ requireProperty $ do
        w0 <- forAll $ G.word32 R.constantBounded
        w1 <- forAll $ G.word32 R.constantBounded

        let w2 = leConcat w0 w1
        let Triplet _ e0 maxE0 = minMaxExcess0 w0
        let Triplet _ _  maxE1 = minMaxExcess0 w1
        let Triplet _ _  maxE2 = minMaxExcess0 w2
        maxE2 === maxE0 `max` (maxE1 + e0)
      it "minE2 == minE0 `min` (minE1 + e0) via vector" $ requireProperty $ do
        w0 <- forAll $ G.word32 R.constantBounded
        w1 <- forAll $ G.word32 R.constantBounded

        let w2 = DVS.fromList [w0, w1]
        let Triplet minE0 e0 _ = minMaxExcess0 w0
        let Triplet minE1 _  _ = minMaxExcess0 w1
        let Triplet minE2 _  _ = minMaxExcess0 w2
        minE2 === minE0 `min` (minE1 + e0)
      it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $ requireProperty $ do
        w0 <- forAll $ G.word32 R.constantBounded
        w1 <- forAll $ G.word32 R.constantBounded

        let w2 = DVS.fromList [w0, w1]
        let Triplet _ e0 maxE0 = minMaxExcess0 w0
        let Triplet _ _  maxE1 = minMaxExcess0 w1
        let Triplet _ _  maxE2 = minMaxExcess0 w2
        maxE2 === maxE0 `max` (maxE1 + e0)
  describe "For Word64" $ do
    it "Excess should be between min excess and max excess" $ requireProperty $ do
      w <- forAll $ G.word64 R.constantBounded
      let Triplet minE e maxE = minMaxExcess0 w
      assert $ minE <= e && e <= maxE
    it "minE2 == minE0 `min` (minE1 + e0) via vector" $ requireProperty $ do
      w0 <- forAll $ G.word64 R.constantBounded
      w1 <- forAll $ G.word64 R.constantBounded
      let w2 = DVS.fromList [w0, w1]
      let Triplet minE0 e0 _ = minMaxExcess0 w0
      let Triplet minE1 _  _ = minMaxExcess0 w1
      let Triplet minE2 _  _ = minMaxExcess0 w2
      minE2 === minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $ requireProperty $ do
      w0 <- forAll $ G.word64 R.constantBounded
      w1 <- forAll $ G.word64 R.constantBounded
      let w2 = DVS.fromList [w0, w1]
      let Triplet _ e0 maxE0 = minMaxExcess0 w0
      let Triplet _ _  maxE1 = minMaxExcess0 w1
      let Triplet _ _  maxE2 = minMaxExcess0 w2
      maxE2 === maxE0 `max` (maxE1 + e0)
  describe "Equivalent to native implementation" $ do
    it "For Word8" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      minMaxExcess0 w === minMaxExcess0 (Naive w)
    it "For Word16" $ requireProperty $ do
      w <- forAll $ G.word16 R.constantBounded
      minMaxExcess0 w === minMaxExcess0 (Naive w)
    it "For Word32" $ requireProperty $ do
      w <- forAll $ G.word32 R.constantBounded
      minMaxExcess0 w === minMaxExcess0 (Naive w)
    it "For Word64" $ requireProperty $ do
      w <- forAll $ G.word64 R.constantBounded
      minMaxExcess0 w === minMaxExcess0 (Naive w)
  describe "Equivalent to word8Excess0' implementation" $ do
    it "For word8" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      let Triplet  lo0 ex0 hi0 = minMaxExcess0     w
      let Triplet8 lo1 ex1 hi1 = genWord8Excess0 8 w
      lo0 === fromIntegral lo1
      ex0 === fromIntegral ex1
      hi0 === fromIntegral hi1
