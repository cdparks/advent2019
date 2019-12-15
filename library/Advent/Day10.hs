module Advent.Day10
  ( main
  )
where

import Advent.Prelude

import Advent.List (focus)
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

parse :: Text -> [Point Int]
parse = concat . zipWith parseLine [0 ..] . lines
 where
  parseLine y = catMaybes . zipWith (`plot` y) [0 ..] . unpack
  plot x y = \case
    '#' -> pure $ Point x y
    _ -> Nothing

nth :: Int -> HashMap (Point Int) [Point Int] -> Point Int
nth n points = spiral points !! (n - 1)

-- brittany-disable-next-binding

spiral :: HashMap (Point Int) [Point Int] -> [Point Int]
spiral =
  concat
  . transpose
  . fmap snd
  . sortOn (firingAngle . fst)
  . HashMap.toList

-- brittany-disable-next-binding

partition :: Point Int -> [Point Int] -> HashMap (Point Int) [Point Int]
partition origin =
  HashMap.map (sortOn distSquared)
  . HashMap.fromListWith (<>)
  . fmap (collapse . shift origin)

firingAngle :: Point Int -> Double
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

distSquared :: Point Int -> Int
distSquared (Point x y) = x * x + y * y

shift :: Point Int -> Point Int -> Point Int
shift origin p = p - origin

unshift :: Point Int -> Point Int -> Point Int
unshift origin p = p + origin

collapse :: Point Int -> (Point Int, [Point Int])
collapse point@(Point x y) = (collapsed, [point])
 where
  collapsed = Point (x `div` d) (y `div` d)
  d = gcd x y

animate :: Point Int -> HashMap (Point Int) [Point Int] -> FilePath -> IO ()
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

getBounds :: [Point Int] -> (Int, Int)
getBounds = foldl' step (0, 0)
  where step (!mx, !my) (Point x y) = (max mx x, max my y)

generateFrames
  :: Int -> Point Int -> HashMap (Point Int) [Point Int] -> [Image PixelRGB8]
generateFrames scale origin points = loop (HashSet.fromList allPoint) targets
 where
  (mx, my) = getBounds allPoint

  targets :: [Point Int]
  targets = unshift origin <$> spiral points

  allPoint :: [Point Int]
  allPoint = origin : do
    bucket <- HashMap.elems points
    point <- bucket
    pure $ unshift origin point

  toLine :: Point Int -> HashSet (Point Int)
  toLine = HashSet.fromList . fmap (unshift origin) . plotLine 0 . shift origin

  loop :: HashSet (Point Int) -> [Point Int] -> [Image PixelRGB8]
  loop _ [] = []
  loop !space (t : ts) =
    generateImage
        (shade (Scene scale origin (toLine t) t space))
        (mx * scale)
        (my * scale)
      : loop (HashSet.delete t space) ts

data Scene = Scene
  { _scale :: Int
  , _origin :: Point Int
  , _line :: HashSet (Point Int)
  , _target :: Point Int
  , _points :: HashSet (Point Int)
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

plotLine :: Point Int -> Point Int -> [Point Int]
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
