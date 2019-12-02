module Advent.Dispatch
  ( getDay
  , Day(..)
  )
where

import Advent.Prelude

import System.Environment (getArgs, getProgName)
import Data.Char (isSpace)

data Day
  = Day1
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
  die $ name <> " day[N]N"
  error "unreachable"

parseDay :: String -> Maybe Day
parseDay (c : cs) | isSpace c = parseDay cs
parseDay ('d' : 'a' : 'y' : cs) | Just 1 <- readMaybe @Int cs = pure Day1
parseDay _ = Nothing
