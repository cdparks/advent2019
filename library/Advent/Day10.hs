module Advent.Day10
  ( main
  )
where

import Advent.Prelude

import Advent.List (focus)
import Advent.Vec2 (Vec2(..))
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
      let Vec2 x y = unshift location $ nth 200 asteroids
      print $ x * 100 + y
  traverse_ (animate location asteroids) =<< lookupEnv "SAVE"

parse :: Text -> [Vec2 Int]
parse = concat . zipWith parseLine [0 ..] . lines
 where
  parseLine y = catMaybes . zipWith (`plot` y) [0 ..] . unpack
  plot x y = \case
    '#' -> pure $ Vec2 x y
    _ -> Nothing

nth :: Int -> HashMap (Vec2 Int) [Vec2 Int] -> Vec2 Int
nth n points = spiral points !! (n - 1)

-- brittany-disable-next-binding

spiral :: HashMap (Vec2 Int) [Vec2 Int] -> [Vec2 Int]
spiral =
  concat
  . transpose
  . fmap snd
  . sortOn (firingAngle . fst)
  . HashMap.toList

-- brittany-disable-next-binding

partition :: Vec2 Int -> [Vec2 Int] -> HashMap (Vec2 Int) [Vec2 Int]
partition origin =
  HashMap.map (sortOn distSquared)
  . HashMap.fromListWith (<>)
  . fmap (collapse . shift origin)

firingAngle :: Vec2 Int -> Double
firingAngle (Vec2 x y)
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

distSquared :: Vec2 Int -> Int
distSquared (Vec2 x y) = x * x + y * y

shift :: Vec2 Int -> Vec2 Int -> Vec2 Int
shift origin p = p - origin

unshift :: Vec2 Int -> Vec2 Int -> Vec2 Int
unshift origin p = p + origin

collapse :: Vec2 Int -> (Vec2 Int, [Vec2 Int])
collapse point@(Vec2 x y) = (collapsed, [point])
 where
  collapsed = Vec2 (x `div` d) (y `div` d)
  d = gcd x y

animate :: Vec2 Int -> HashMap (Vec2 Int) [Vec2 Int] -> FilePath -> IO ()
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

getBounds :: [Vec2 Int] -> (Int, Int)
getBounds = foldl' step (0, 0)
  where step (!mx, !my) (Vec2 x y) = (max mx x, max my y)

generateFrames
  :: Int -> Vec2 Int -> HashMap (Vec2 Int) [Vec2 Int] -> [Image PixelRGB8]
generateFrames scale origin points = loop (HashSet.fromList allVec2) targets
 where
  (mx, my) = getBounds allVec2

  targets :: [Vec2 Int]
  targets = unshift origin <$> spiral points

  allVec2 :: [Vec2 Int]
  allVec2 = origin : do
    bucket <- HashMap.elems points
    point <- bucket
    pure $ unshift origin point

  toLine :: Vec2 Int -> HashSet (Vec2 Int)
  toLine = HashSet.fromList . fmap (unshift origin) . plotLine 0 . shift origin

  loop :: HashSet (Vec2 Int) -> [Vec2 Int] -> [Image PixelRGB8]
  loop _ [] = []
  loop !space (t : ts) =
    generateImage
        (shade (Scene scale origin (toLine t) t space))
        (mx * scale)
        (my * scale)
      : loop (HashSet.delete t space) ts

data Scene = Scene
  { _scale :: Int
  , _origin :: Vec2 Int
  , _line :: HashSet (Vec2 Int)
  , _target :: Vec2 Int
  , _points :: HashSet (Vec2 Int)
  }

shade :: Scene -> Int -> Int -> PixelRGB8
shade Scene {..} !x !y
  | point == _origin = PixelRGB8 255 0 0
  | point == _target = PixelRGB8 0 255 0
  | inLine = PixelRGB8 0 0 255
  | alive = PixelRGB8 255 255 255
  | otherwise = PixelRGB8 0 0 0
 where
  point = Vec2 (x `div` _scale) (y `div` _scale)
  alive = point `HashSet.member` _points
  inLine = point `HashSet.member` _line

plotLine :: Vec2 Int -> Vec2 Int -> [Vec2 Int]
plotLine (Vec2 x0 y0) (Vec2 x1 y1) = loop x0 y0 $ dx + dy
 where
  dx = abs $ x1 - x0
  sx = if x0 < x1 then 1 else negate 1

  dy = negate $ abs $ y1 - y0
  sy = if y0 < y1 then 1 else negate 1

  loop !x !y !err
    | x == x1 && y == y1 = []
    | otherwise = Vec2 x y : loop x' y' err'
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
