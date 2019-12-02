module Main
  ( main
  )
where

import Advent.Prelude

import qualified Pipes.Prelude as Pipes

-- Before accounting for fuel mass:
-- > stack exec day1 < inputs/day1.txt
-- > 3342351
--
-- After accounting for fuel mass:
-- > stack exec day1 < inputs/day1.txt
-- > 5010664

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
