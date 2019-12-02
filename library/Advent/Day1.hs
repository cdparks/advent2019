module Advent.Day1
  ( main
  ) where

import Advent.Prelude

import qualified Pipes.Prelude as Pipes

main :: IO ()
main = do
  total <-
    Pipes.sum
    $ Pipes.map fuelNeeded
    <-< Pipes.mapFoldable readMaybe
    <-< Pipes.stdinLn
  print total

fuelNeeded :: Int -> Int
fuelNeeded = sum . takeWhile (> 0) . iterate step . step
  where step !mass = max 0 (mass `div` 3 - 2)
