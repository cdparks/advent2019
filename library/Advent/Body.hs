module Advent.Body
  ( Body(..)
  , pos
  , vel
  , static
  , move
  )
where

import Advent.Prelude

import Lens.Micro
import Text.Show

data Body p = Body { _pos :: p, _vel :: p }
  deriving (Eq, Functor, Foldable, Traversable)

instance Show p => Show (Body p) where
  showsPrec _ Body {..} =
    showString "{pos="
      . shows _pos
      . showString ", vel="
      . shows _vel
      . showChar '}'

pos :: Lens' (Body p) p
pos = lens _pos $ \b v -> b { _pos = v }

vel :: Lens' (Body p) p
vel = lens _vel $ \b v -> b { _vel = v }

static :: Num p => p -> Body p
static p = Body p 0

move :: Num p => Body p -> Body p
move b = b & pos +~ (b ^. vel)
