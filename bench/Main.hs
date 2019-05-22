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
  where go :: Int64 -> Word64 -> Int64
        go a b = a + (leh0Lo 64 c) + (leh0Ex 64 c) + (leh0Hi 64 c)
          where c = fromIntegral b :: Word64

runLeh1Vector :: DVS.Vector Word64 -> IO ()
runLeh1Vector v = do
  let !_ = DVS.foldl go 0 v

  return ()
  where go :: Int64 -> Word64 -> Int64
        go a b = a + (leh1Lo 64 c) + (leh1Ex 64 c) + (leh1Hi 64 c)
          where c = fromIntegral b :: Word64

makeBenchExcess :: IO [Benchmark]
makeBenchExcess = return $
  [ env (setupEnvExcess (1024 * 1024)) $ \v -> bgroup "Leh Vector"
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
