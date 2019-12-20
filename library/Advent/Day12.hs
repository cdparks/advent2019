module Advent.Day12
  ( main
  )
where

import Advent.Prelude

import Advent.Body (Body(..))
import qualified Advent.Body as Body
import Advent.List (focus)
import Advent.Text (readCommaSep)
import Advent.Vec3 (Vec3(..))
import qualified Advent.Vec3 as Vec3
import Data.List ((!!))
import Data.Maybe (fromJust)
import Data.Text.IO (getContents)
import Lens.Micro

main :: Part -> IO ()
main part = do
  moons <- parse <$> getContents
  let steps = iterate simulate $ Body.static <$> moons
  case part of
    Part1 -> print $ sum $ totalEnergy <$> steps !! 1000
    Part2 -> print $ fromJust $ duplicate steps

parse :: Text -> [Vec3 Int]
parse = mapMaybe parseVec3 . lines

parseVec3 :: Text -> Maybe (Vec3 Int)
parseVec3 t = case readCommaSep t of
  [a, b, c] -> Just $ Vec3 a b c
  _ -> Nothing

type Moon = Body (Vec3 Int)

simulate :: [Moon] -> [Moon]
simulate = applyVelocity . applyGravity

totalEnergy :: Moon -> Int
totalEnergy moon = energy Body.pos * energy Body.vel
  where energy = sum . Vec3.scalars . abs . (moon ^.)

applyGravity :: [Moon] -> [Moon]
applyGravity moons = uncurry (foldl' step) <$> focus moons
  where step moon other = attract moon $ other ^. Body.pos

attract :: Moon -> Vec3 Int -> Moon
attract moon other = moon & Body.vel +~ signum (other - moon ^. Body.pos)

applyVelocity :: [Moon] -> [Moon]
applyVelocity = map Body.move

duplicate :: [[Moon]] -> Maybe Integer
duplicate steps = do
  l <- stepsToZero Vec3.x
  m <- stepsToZero Vec3.y
  n <- stepsToZero Vec3.z
  pure $ 2 * (lcm l m `lcm` lcm m n)
 where
  stepsToZero :: Lens' (Vec3 Int) Int -> Maybe Integer
  stepsToZero axis = fst <$> find ((== 0) . velocity1D axis . snd) labeled

  labeled = drop 1 $ zip [0 ..] steps

velocity1D :: Lens' (Vec3 Int) Int -> [Moon] -> Int
velocity1D axis = sum . map (\moon -> abs $ moon ^. Body.vel . axis)
