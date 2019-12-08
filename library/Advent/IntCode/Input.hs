module Advent.IntCode.Input
  ( Input(..)
  , HasInput(..)
  , parse
  , read
  )
where

import Advent.Prelude

import Advent.Text
import Lens.Micro (Lens')
import Lens.Micro.Mtl (use, (.=))

newtype Input = Input { unInput :: [Int] }

parse :: Text -> Input
parse = Input . readCommaSep

read :: (MonadState s m, HasInput s) => m Int
read = use inputLens >>= \case
  Input [] -> error "Premature end of input"
  Input (i : is) -> do
    inputLens .= Input is
    pure i

class HasInput s where
  inputLens :: Lens' s Input

instance HasInput Input where
  inputLens = id
  {-# INLINE inputLens #-}
