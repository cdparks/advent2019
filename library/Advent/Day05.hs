module Advent.Day05
  ( main
  )
where

import Advent.Prelude

import Advent.IntCode
import qualified Advent.IntCode.Input as Input
import Advent.IntCode.Output (Output(..))
import qualified Advent.IntCode.Program as Program

main :: IO ()
main = do
  program <- Program.parse <$> getLine
  input <- Input.parse <$> getLine
  traverse_ print $ unOutput $ snd $ run input program
