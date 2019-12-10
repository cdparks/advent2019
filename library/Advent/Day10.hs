module Advent.Day10
  ( main
  )
where

import Advent.Prelude

import Advent.Point (Point(..))
import Data.Foldable (maximumBy)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn, transpose, (!!))
import Data.Text (Text, unpack)
import Data.Text.IO (getContents)
import GHC.Float (atan2)

main :: Part -> IO ()
main part = do
  points <- parse <$> getContents
  let
    (location, asteroids) = maximumBy (comparing $ HashMap.size . snd) $ do
      (origin, rest) <- focus points
      pure (origin, partition origin rest)
  case part of
    Part1 -> print $ HashMap.size asteroids
    Part2 -> do
      let Point x y = unshift location $ nth 200 asteroids
      print $ x * 100 + y

parse :: Text -> [Point]
parse = concat . zipWith parseLine [0 ..] . lines
 where
  parseLine y = catMaybes . zipWith (`plot` y) [0 ..] . unpack
  plot x y = \case
    '#' -> pure $ Point x y
    _ -> Nothing

nth :: Int -> HashMap Point [Point] -> Point
nth n points = spiral points !! (n - 1)

-- brittany-disable-next-binding

spiral :: HashMap Point [Point] -> [Point]
spiral =
  concat
  . transpose
  . fmap snd
  . sortOn (firingAngle . fst)
  . HashMap.toList

-- brittany-disable-next-binding

partition :: Point -> [Point] -> HashMap Point [Point]
partition origin =
  HashMap.map (sortOn distSquared)
  . HashMap.fromListWith (<>)
  . fmap (collapse . shift origin)

firingAngle :: Point -> Double
firingAngle (Point x y)
  | theta < 0 = convert $ theta + 360
  | otherwise = convert $ theta
 where
  theta = atan2 @Double (fromIntegral y) (fromIntegral x) * (180 / pi)
  convert d = (d + 90) `fmod` 360

fmod :: Double -> Double -> Double
fmod x m
  | x < 0 = fmod (negate x) m
  | x < m = x
  | otherwise = fmod (x - m) m

distSquared :: Point -> Int
distSquared (Point x y) = x * x + y * y

shift :: Point -> Point -> Point
shift (Point dx dy) (Point x y) = Point (x - dx) (y - dy)

unshift :: Point -> Point -> Point
unshift (Point dx dy) (Point x y) = Point (x + dx) (y + dy)

collapse :: Point -> (Point, [Point])
collapse point@(Point x y) = (collapsed, [point])
 where
  collapsed = Point (x `div` d) (y `div` d)
  d = gcd x y

focus :: [a] -> [(a, [a])]
focus = go []
 where
  go _ [] = []
  go xs (y : ys) = (y, xs <> ys) : go (y : xs) ys
