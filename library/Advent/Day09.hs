module Advent.Day09
  ( main
  )
where

import Advent.Prelude

import Advent.IntCode
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (parse)

main :: Part -> IO ()
main part = do
  program <- parse <$> getLine
  traverse_ print $ unOutput $ snd $ run (Input [input]) program
 where
  input = case part of
    Part1 -> 1
    Part2 -> 2
