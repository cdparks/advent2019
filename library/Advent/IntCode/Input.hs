module Advent.IntCode.Input
  ( Input(..)
  , parse
  , RawInput
  , new
  , read
  )
where

import Advent.Prelude

import Advent.Text
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

newtype Input = Input { unInput :: [Int] }

parse :: Text -> Input
parse = Input . readCommaSep

newtype RawInput s = RawInput (STRef s [Int])

new :: Input -> ST s (RawInput s)
new (Input is) = RawInput <$> newSTRef is

read :: RawInput s -> ST s Int
read (RawInput ref) = do
  input <- readSTRef ref
  case input of
    [] -> error "Premature end of input"
    i : is -> do
      writeSTRef ref is
      pure i
