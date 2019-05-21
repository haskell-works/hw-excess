module HaskellWorks.Data.Excess.Internal
  ( word8Excess0
  , word8Excess0Max
  , word8Excess0Min
  , word8Excess1
  , word8Excess1Max
  , word8Excess1Min
  ) where

import qualified Data.Vector.Storable as DVS

word8Excess0Min :: DVS.Vector Int
word8Excess0Min =  DVS.fromList
  [  0, -1,  0, -2,  0, -1, -1, -3,  0, -1,  0, -2,  0, -2, -2, -4
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1, -1, -3, -1, -3, -3, -5
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1,  0, -2,  0, -2, -2, -4
  ,  0, -1,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1,  0, -2,  0, -2, -2, -4
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1, -1, -3, -1, -3, -3, -5
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1, -1, -3, -1, -3, -3, -5
  ,  0, -1, -1, -3, -1, -3, -3, -5, -1, -3, -3, -5, -3, -5, -5, -7
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1,  0, -2,  0, -2, -2, -4
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1, -1, -3, -1, -3, -3, -5
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1,  0, -2,  0, -2, -2, -4
  ,  0, -1,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  0, -1,  0, -2,  0, -1, -1, -3,  0, -1,  0, -2,  0, -2, -2, -4
  ,  0, -1,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  0, -1,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  0, -2, -2, -4, -2, -4, -4, -6, -2, -4, -4, -6, -4, -6, -6, -8
  ]
{-# NOINLINE word8Excess0Min #-}

word8Excess0 :: DVS.Vector Int
word8Excess0 =  DVS.fromList
  [  8,  6,  6,  4,  6,  4,  4,  2,  6,  4,  4,  2,  4,  2,  2,  0
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  0, -2
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  0, -2
  ,  4,  2,  2,  0,  2,  0,  0, -2,  2,  0,  0, -2,  0, -2, -2, -4
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  0, -2
  ,  4,  2,  2,  0,  2,  0,  0, -2,  2,  0,  0, -2,  0, -2, -2, -4
  ,  4,  2,  2,  0,  2,  0,  0, -2,  2,  0,  0, -2,  0, -2, -2, -4
  ,  2,  0,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  0, -2
  ,  4,  2,  2,  0,  2,  0,  0, -2,  2,  0,  0, -2,  0, -2, -2, -4
  ,  4,  2,  2,  0,  2,  0,  0, -2,  2,  0,  0, -2,  0, -2, -2, -4
  ,  2,  0,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  4,  2,  2,  0,  2,  0,  0, -2,  2,  0,  0, -2,  0, -2, -2, -4
  ,  2,  0,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  2,  0,  0, -2,  0, -2, -2, -4,  0, -2, -2, -4, -2, -4, -4, -6
  ,  0, -2, -2, -4, -2, -4, -4, -6, -2, -4, -4, -6, -4, -6, -6, -8
  ]
{-# NOINLINE word8Excess0 #-}

word8Excess0Max :: DVS.Vector Int
word8Excess0Max =  DVS.fromList
  [  8,  6,  6,  4,  6,  4,  4,  2,  6,  4,  4,  2,  4,  2,  2,  0
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  1,  0
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  1,  0
  ,  4,  2,  2,  0,  2,  0,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  1,  0
  ,  4,  2,  2,  0,  2,  0,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  5,  3,  3,  1,  3,  1,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  4,  2,  2,  0,  2,  0,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  7,  5,  5,  3,  5,  3,  3,  1,  5,  3,  3,  1,  3,  1,  1,  0
  ,  5,  3,  3,  1,  3,  1,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  5,  3,  3,  1,  3,  1,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  4,  2,  2,  0,  2,  0,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  6,  4,  4,  2,  4,  2,  2,  0,  4,  2,  2,  0,  2,  0,  1,  0
  ,  4,  2,  2,  0,  2,  0,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  5,  3,  3,  1,  3,  1,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ,  4,  2,  2,  0,  2,  0,  1,  0,  3,  1,  1,  0,  2,  0,  1,  0
  ]
{-# NOINLINE word8Excess0Max #-}

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
{-# NOINLINE word8Excess1Min #-}

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
{-# NOINLINE word8Excess1 #-}

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
{-# NOINLINE word8Excess1Max #-}
