module Advent.Day07
  ( main
  )
where

import Advent.Prelude hiding (last)

import Advent.IntCode
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (parse)
import Data.Foldable (maximum)
import Data.List (last)

main :: Part -> IO ()
main part = do
  program <- parse <$> getLine
  print $ maximum $ do
    phases <- permutations range
    let signal = foldr (step program) (0 : signal) phases
    pure $ last signal
 where
  step program phase signal =
    unOutput $ snd $ run (Input (phase : signal)) program
  range = case part of
    Part1 -> [0 .. 4]
    Part2 -> [5 .. 9]
