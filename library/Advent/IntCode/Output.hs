module Advent.IntCode.Output
  ( Output(..)
  , RawOutput
  , new
  , write
  , freeze
  )
where

import Advent.Prelude

import Control.Monad.ST (ST)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)

newtype Output = Output { unOutput :: [Int] }
  deriving newtype Show

newtype RawOutput s = RawOutput (STRef s [Int])

new :: ST s (RawOutput s)
new = RawOutput <$> newSTRef []

write :: RawOutput s -> Int -> ST s ()
write (RawOutput ref) i = modifySTRef' ref (i :)

freeze :: RawOutput s -> ST s Output
freeze (RawOutput ref) = Output . reverse <$> readSTRef ref
