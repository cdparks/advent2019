module Advent.Prelude
  ( module Relude
  , module Pipes
  , Part(..)
  )
where

import Pipes hiding (Proxy)
import Relude

data Part = Part1 | Part2
