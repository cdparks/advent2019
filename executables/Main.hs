module Main
  ( main
  )
where

import Advent.Prelude

import qualified Advent.Day1 as Day1
import Advent.Dispatch (Day(..), getDay)

main :: IO ()
main = do
  day <- getDay
  case day of
    Day1 -> Day1.main
