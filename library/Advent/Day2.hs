module Advent.Day2
  ( main
  )
where

import Advent.Prelude

import Advent.IntCode (Noun(..), Verb(..))
import qualified Advent.IntCode as IntCode
import Data.Text.IO (getContents)

main :: IO ()
main = do
  program <- IntCode.parse <$> getContents
  for_ [0 .. 99] $ \noun -> for_ [0 .. 99] $ \verb -> do
    let memory = IntCode.run (Noun noun) (Verb verb) program
    when (memory `IntCode.at` 0 == 19690720) $ print $ 100 * noun + verb
