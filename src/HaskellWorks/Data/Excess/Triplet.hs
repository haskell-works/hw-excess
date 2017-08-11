{-# OPTIONS_GHC-funbox-strict-fields #-}

module HaskellWorks.Data.Excess.Triplet
  ( Triplet(..)
  ) where

import Control.Applicative (liftA3)
import Foreign.Storable    (Storable (..))

import qualified Foreign.Storable.Record as Store

data Triplet = Triplet
  { tripletMinExcess :: !Int
  , tripletAllExcess :: !Int
  , tripletMaxExcess :: !Int
  } deriving (Eq, Show)

instance Storable Triplet where
  sizeOf    = Store.sizeOf storeTriple
  alignment = Store.alignment storeTriple
  peek      = Store.peek storeTriple
  poke      = Store.poke storeTriple

storeTriple :: Store.Dictionary Triplet
storeTriple = Store.run $ liftA3 Triplet
  (Store.element tripletMinExcess)
  (Store.element tripletAllExcess)
  (Store.element tripletMaxExcess)
