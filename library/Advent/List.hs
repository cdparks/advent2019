module Advent.List
  ( focus
  )
where

import Advent.Prelude

focus :: [a] -> [(a, [a])]
focus = go []
 where
  go _ [] = []
  go xs (y : ys) = (y, xs <> ys) : go (y : xs) ys
