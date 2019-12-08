module Advent.IntCode.Output
  ( Output(..)
  )
where

import Advent.Prelude

newtype Output = Output { unOutput :: [Int] }
  deriving newtype Show
