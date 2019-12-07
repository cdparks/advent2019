module Advent.IntCode.Address
  ( Address
  , from
  , asInt
  )
where

import Advent.Prelude

import GHC.Show (Show(..), showChar, shows)

newtype Address = Address Int
  deriving newtype (Eq, Ord, Num)

instance Show Address where
  showsPrec _ (Address i) = showChar '#' . shows i

from :: Int -> Address
from = coerce
{-# INLINE from #-}

asInt :: Address -> Int
asInt = coerce
{-# INLINE asInt #-}
