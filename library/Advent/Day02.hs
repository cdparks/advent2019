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

main :: IO ()
main = do
  program <- parse <$> getContents
  for_ [0 .. 99] $ \noun -> for_ [0 .. 99] $ \verb -> do
    let
      memory = fst $ run (Input []) $ patch [(1, Noun noun), (2, Verb verb)] program
    when (memory ! 0 == 19690720) $ print $ 100 * noun + verb
