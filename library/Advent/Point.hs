{-# LANGUAGE StrictData #-}

module Advent.Point
  ( Point(..)
  , x
  , y
  )
where

import Advent.Prelude

import GHC.Generics (Generic)
import Lens.Micro
import Text.Show

data Point = Point
  { _x :: {-# UNPACK #-} Int
  , _y :: {-# UNPACK #-} Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Show Point where
  showsPrec _ Point {..} =
    showChar '(' . shows _x . showString ", " . shows _y . showChar ')'

x :: Lens' Point Int
x = lens _x $ \p v -> p { _x = v }

y :: Lens' Point Int
y = lens _y $ \p v -> p { _y = v }
