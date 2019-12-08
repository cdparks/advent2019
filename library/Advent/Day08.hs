module Advent.Day08
  ( main
  )
where

import Advent.Prelude

import Data.Char (ord)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as U

main :: IO ()
main = do
  layers <- parse n <$> getLine
  render w $ toColor layers <$> [0 .. (n - 1)]
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
new = U.fromList . fmap (subtract zero . ord) . T.unpack where zero = ord '0'

type Layer = Vector Int

data Color = Black | White | Transparent
  deriving (Enum)
