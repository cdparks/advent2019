module Advent.Day10
  ( main
  )
where

import Advent.Prelude

import Advent.Point (Point(..))
import Codec.Picture
import Data.Foldable (maximumBy)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (sortOn, transpose, (!!))
import Data.Text (Text, unpack)
import Data.Text.IO (getContents)
import GHC.Float (atan2)
import System.Environment (lookupEnv)

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
  traverse_ (animate location asteroids) =<< lookupEnv "SAVE"

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

animate :: Point -> HashMap Point [Point] -> FilePath -> IO ()
animate origin points filename = do
  scale <- fromMaybe 12 <$> lookupNum "SCALE"
  delay <- fromMaybe 8 <$> lookupNum "DELAY"
  putStrLn $ "Saving animation to " <> filename
  let
    frames = generateFrames scale origin points
    eAction = writeGifAnimation filename delay LoopingForever frames
  case eAction of
    Left err -> putStrLn err
    Right action -> do
      action
      putStrLn "Done"
  where lookupNum name = (readMaybe =<<) <$> lookupEnv name

getBounds :: [Point] -> (Int, Int)
getBounds = foldl' step (0, 0)
  where step (!mx, !my) (Point x y) = (max mx x, max my y)

generateFrames :: Int -> Point -> HashMap Point [Point] -> [Image PixelRGB8]
generateFrames scale origin points = loop (HashSet.fromList allPoints) targets
 where
  (mx, my) = getBounds allPoints

  targets :: [Point]
  targets = unshift origin <$> spiral points

  allPoints :: [Point]
  allPoints = origin : do
    bucket <- HashMap.elems points
    point <- bucket
    pure $ unshift origin point

  toLine :: Point -> HashSet Point
  toLine =
    HashSet.fromList
      . fmap (unshift origin)
      . plotLine (Point 0 0)
      . shift origin

  loop :: HashSet Point -> [Point] -> [Image PixelRGB8]
  loop _ [] = []
  loop !space (t : ts) =
    generateImage
        (shade (Scene scale origin (toLine t) t space))
        (mx * scale)
        (my * scale)
      : loop (HashSet.delete t space) ts

data Scene = Scene
  { _scale :: Int
  , _origin :: Point
  , _line :: HashSet Point
  , _target :: Point
  , _points :: HashSet Point
  }

shade :: Scene -> Int -> Int -> PixelRGB8
shade Scene {..} !x !y
  | point == _origin = PixelRGB8 255 0 0
  | point == _target = PixelRGB8 0 255 0
  | inLine = PixelRGB8 0 0 255
  | alive = PixelRGB8 255 255 255
  | otherwise = PixelRGB8 0 0 0
 where
  point = Point (x `div` _scale) (y `div` _scale)
  alive = point `HashSet.member` _points
  inLine = point `HashSet.member` _line

plotLine :: Point -> Point -> [Point]
plotLine (Point x0 y0) (Point x1 y1) = loop x0 y0 $ dx + dy
 where
  dx = abs $ x1 - x0
  sx = if x0 < x1 then 1 else negate 1

  dy = negate $ abs $ y1 - y0
  sy = if y0 < y1 then 1 else negate 1

  loop !x !y !err
    | x == x1 && y == y1 = []
    | otherwise = Point x y : loop x' y' err'
   where
    err2 = 2 * err
    dyErr = err2 >= dy
    dxErr = err2 <= dx
    x' = x + iff dyErr sx
    y' = y + iff dxErr sy
    err' = err + iff dyErr dy + iff dxErr dx

iff :: Bool -> Int -> Int
iff cond n = if cond then n else 0
{-# INLINE iff #-}
