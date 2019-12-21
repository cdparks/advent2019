module Advent.Text
  ( readCommaSep
  , toDigit
  , fromDigit
  )
where

import Advent.Prelude

import Data.Text (Text)
import qualified Data.Text as T

readCommaSep :: Read a => Text -> [a]
readCommaSep = mapMaybe (readMaybe . T.unpack) . T.splitOn ","

toDigit :: Char -> Int
toDigit = subtract zero . ord

fromDigit :: Int -> Char
fromDigit = chr . (+ zero)

zero :: Int
zero = ord '0'
