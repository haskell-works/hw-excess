{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.List
import Data.Word
import Foreign
import HaskellWorks.Data.Excess.Internal.Partial.Leh0
import HaskellWorks.Data.Excess.Internal.Partial.Leh1
import HaskellWorks.Data.Excess.Mem0
import HaskellWorks.Data.Excess.Mem1
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

runLeh0Vector :: DVS.Vector Word64 -> IO ()
runLeh0Vector v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where c = fromIntegral b :: Word64
                Triplet lo ex hi = leh0 64 c

runLeh1Vector :: DVS.Vector Word64 -> IO ()
runLeh1Vector v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where c = fromIntegral b :: Word64
                Triplet lo ex hi = leh1 64 c

runMem0VectorElems :: DVS.Vector Word64 -> IO ()
runMem0VectorElems v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where Triplet lo ex hi  = mem0 (b :: Word64)

runMem1VectorElems :: DVS.Vector Word64 -> IO ()
runMem1VectorElems v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int -> Word64 -> Int
        go a b = a + lo + ex + hi
          where Triplet lo ex hi  = mem1 (b :: Word64)

runMem0Vector :: DVS.Vector Word64 -> IO ()
runMem0Vector v = do
  let !_ = mem1 v

  return ()

runMem1Vector :: DVS.Vector Word64 -> IO ()
runMem1Vector v = do
  let !_ = mem1 v

  return ()

makeBenchExcess :: IO [Benchmark]
makeBenchExcess = return $
  [ env (setupEnvExcess (1024 * 1024)) $ \v -> bgroup "Mem Vector"
    [ bench "Mem0" (whnfIO (runMem0VectorElems v))
    , bench "Mem1" (whnfIO (runMem1VectorElems v))
    , bench "Mem0" (whnfIO (runMem0Vector      v))
    , bench "Mem1" (whnfIO (runMem1Vector      v))
    ]
  , env (setupEnvExcess (1024 * 1024)) $ \v -> bgroup "Leh Vector"
    [ bench "Leh0" (whnfIO (runLeh0Vector v))
    , bench "Leh1" (whnfIO (runLeh1Vector v))
    ]
  ]

main :: IO ()
main = do
  benchmarks <- mconcat <$> sequence
    [ makeBenchExcess
    ]
  defaultMain benchmarks
