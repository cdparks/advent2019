module Main
  ( main
  )
where

import Advent.Prelude

import qualified Advent.Day01 as Day01
import qualified Advent.Day02 as Day02
import qualified Advent.Day03 as Day03
import qualified Advent.Day04 as Day04
import Advent.Dispatch (Day(..), getDay)

main :: IO ()
main = do
  day <- getDay
  case day of
    Day01 -> Day01.main
    Day02 -> Day02.main
    Day03 -> Day03.main
    Day04 -> Day04.main
