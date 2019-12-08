module Advent.Main
  ( main
  )
where

import Advent.Prelude

import qualified Advent.Day01 as Day01
import qualified Advent.Day02 as Day02
import qualified Advent.Day03 as Day03
import qualified Advent.Day04 as Day04
import qualified Advent.Day05 as Day05
import qualified Advent.Day06 as Day06
import qualified Advent.Day07 as Day07
import qualified Advent.Day08 as Day08
import Data.Text (pack, unpack)
import System.Environment (getArgs, getProgName)

getDay :: IO Natural
getDay = do
  args <- getArgs
  case args of
    [day] -> maybe (usage "Could not parse day") pure $ readMaybe day
    _ -> usage "Must specify day"

usage :: Text -> IO a
usage message = do
  name <- pack <$> getProgName
  die $ unpack $ unlines
    [message, "usage: " <> name <> " N", "  where 1 <= N <= 25"]
  error "unreachable" -- Why is die :: IO () instead of IO a?

main :: IO ()
main = do
  day <- getDay
  case day of
    1 -> Day01.main
    2 -> Day02.main
    3 -> Day03.main
    4 -> Day04.main
    5 -> Day05.main
    6 -> Day06.main
    7 -> Day07.main
    8 -> Day08.main
    _
      | day <= 25 -> usage "Day not implemented yet"
      | otherwise -> usage "Day out of range"
