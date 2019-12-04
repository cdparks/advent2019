module Advent.Dispatch
  ( getDay
  , Day(..)
  )
where

import Advent.Prelude

import System.Environment (getArgs, getProgName)

data Day
  = Day01
  | Day02
  | Day03
  deriving (Eq, Show)

getDay :: IO Day
getDay = do
  args <- getArgs
  case args of
    [day] -> maybe usage pure $ parseDay day
    _ -> usage

usage :: IO a
usage = do
  name <- getProgName
  die $ "usage: " <> name <> " N"
  error "unreachable" -- Why is die :: IO () instead of IO a?

parseDay :: String -> Maybe Day
parseDay day = do
  n <- readMaybe @Int day
  case n of
    1 -> pure Day01
    2 -> pure Day02
    3 -> pure Day03
    _ -> Nothing
