module Advent.Day05
  ( main
  )
where

import Advent.Prelude

import Advent.IntCode
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import qualified Advent.IntCode.Program as Program

main :: Part -> IO ()
main part = do
  program <- Program.parse <$> getLine
  traverse_ print $ unOutput $ snd $ run input program
 where
  input = case part of
    Part1 -> Input [1]
    Part2 -> Input [5]
