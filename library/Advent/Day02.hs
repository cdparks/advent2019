module Advent.Day02
  ( main
  )
where

import Advent.Prelude

import Advent.IntCode
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Memory ((!))
import Advent.IntCode.Program
import Data.Text.IO (getContents)

main :: Part -> IO ()
main part = do
  program <- parse <$> getContents
  case part of
    Part1 -> print $ address0 12 2 program
    Part2 -> for_ [0 .. 99] $ \noun -> for_ [0 .. 99] $ \verb ->
      when (address0 noun verb program == 19690720) $ print $ 100 * noun + verb
 where
  address0 noun verb program =
    fst (run (Input []) $ patch [(1, Noun noun), (2, Verb verb)] program) ! 0
