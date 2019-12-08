module Advent.Day01
  ( main
  )
where

import Advent.Prelude

import qualified Pipes.Prelude as Pipes

main :: Part -> IO ()
main part = do
  total <-
    Pipes.sum
    $ Pipes.map calculateFuel
    <-< Pipes.mapFoldable readMaybe
    <-< Pipes.stdinLn
  print total
 where
  calculateFuel = case part of
    Part1 -> fuelForMass
    Part2 -> totalFuelNeeded

fuelForMass :: Int -> Int
fuelForMass !mass = max 0 $ mass `div` 3 - 2

totalFuelNeeded :: Int -> Int
totalFuelNeeded = sum . takeWhile (> 0) . iterate fuelForMass . fuelForMass
