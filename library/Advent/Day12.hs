module Advent.Day12 where

import Advent.Prelude hiding (head, tail)

import Advent.Body (Body(..))
import qualified Advent.Body as Body
import Advent.List (focus)
import Advent.Text (readCommaSep)
import Advent.Vec3 (Vec3(..))
import qualified Advent.Vec3 as Vec3
import Data.List ((!!))
import Data.Text.IO (getContents)
import Lens.Micro

main :: Part -> IO ()
main part = do
  moons <- parse <$> getContents
  case part of
    Part1 -> do
      let steps = iterate simulate $ Body.static <$> moons
      print $ sum $ totalEnergy <$> steps !! 1000
    Part2 -> putStrLn "not implemented"

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
