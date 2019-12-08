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

main :: IO ()
main = do
  program <- parse <$> getLine
  print $ maximum $ do
    phases <- permutations [5 .. 9]
    let signal = foldr (step program) (0 : signal) phases
    pure $ last signal
 where
  step program phase signal =
    unOutput $ snd $ run (Input (phase : signal)) program
