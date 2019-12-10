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

data Point = Point
  { _x :: {-# UNPACK #-} Int
  , _y :: {-# UNPACK #-} Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

x :: Lens' Point Int
x = lens _x $ \p v -> p { _x = v }

y :: Lens' Point Int
y = lens _y $ \p v -> p { _y = v }
