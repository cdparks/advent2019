module Advent.Day08
  ( main
  )
where

import Advent.Prelude

import Data.Foldable (minimumBy)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as U
import Advent.Text (toDigit)

main :: Part -> IO ()
main part = do
  layers <- parse n <$> getLine
  case part of
    Part1 -> print $ checksum $ minimumBy (comparing $ count 0) layers
    Part2 -> render w $ toColor layers <$> [0 .. (n - 1)]
 where
  w = 25
  h = 6
  n = w * h

toColor :: [Layer] -> Int -> Color
toColor layers i = foldl' step Transparent layers
 where
  step Transparent layer = toEnum $ layer ! i
  step color _ = color

render :: Int -> [Color] -> IO ()
render w = traverse_ putStrLn . chunksOf w . fmap toChar
 where
  toChar = \case
    White -> '█'
    _ -> ' '

parse :: Int -> Text -> [Layer]
parse n text = case T.length text of
  0 -> []
  _ -> new layer : parse n rest
  where ~(layer, rest) = T.splitAt n text

new :: Text -> Layer
new = U.fromList . fmap toDigit . T.unpack

checksum :: Layer -> Int
checksum layer = count 1 layer * count 2 layer

count :: Int -> Vector Int -> Int
count n = U.length . U.filter (== n)

type Layer = Vector Int

data Color = Black | White | Transparent
  deriving (Enum)
