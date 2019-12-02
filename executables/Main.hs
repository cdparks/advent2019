module Main
  ( main
  )
where

import Advent.Prelude

import qualified Advent.Day1 as Day1
import qualified Advent.Day2 as Day2
import Advent.Dispatch (Day(..), getDay)

main :: IO ()
main = do
  day <- getDay
  case day of
    Day1 -> Day1.main
    Day2 -> Day2.main
