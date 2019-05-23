{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.List
import Data.Word
import Foreign
import HaskellWorks.Data.Excess.MinMaxExcess0
import HaskellWorks.Data.Excess.MinMaxExcess1
import HaskellWorks.Data.Excess.Triplet
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Vector.Storable     as DVS

setupEnvExcess :: Int -> IO (DVS.Vector Word64)
setupEnvExcess n = do
  lbs <- LBS.readFile "/dev/random"
  return (asVector64 (LBS.toStrict (LBS.take (fromIntegral n) lbs)))

runMinMaxExcess0Vector :: DVS.Vector Word64 -> IO ()
runMinMaxExcess0Vector v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where Triplet lo ex hi  = minMaxExcess0 (b :: Word64)

runMinMaxExcess1Vector :: DVS.Vector Word64 -> IO ()
runMinMaxExcess1Vector v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where Triplet lo ex hi  = minMaxExcess1 (b :: Word64)

makeBenchExcess :: IO [Benchmark]
makeBenchExcess = return $
  [ env (setupEnvExcess (1024 * 1024)) $ \v -> bgroup "MinMaxExcess Vector"
    [ bench "MinMaxExcess0" (whnfIO (runMinMaxExcess0Vector v))
    , bench "MinMaxExcess1" (whnfIO (runMinMaxExcess1Vector v))
    ]
  ]

main :: IO ()
main = do
  benchmarks <- mconcat <$> sequence
    [ makeBenchExcess
    ]
  defaultMain benchmarks
