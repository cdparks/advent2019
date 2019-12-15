{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day03
  ( main
  )
where

import Advent.Prelude hiding (State)

import Advent.Vec2 (Vec2(..))
import qualified Advent.Vec2 as Vec2
import Control.Exception (throwIO)
import Data.Attoparsec.Text hiding (D)
import Data.Foldable (minimum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid (Sum(..))
import Lens.Micro
import System.IO.Error (userError)

main :: Part -> IO ()
main part = do
  wire1 <- parseThrows parseWire =<< getLine
  wire2 <- parseThrows parseWire =<< getLine
  let
    intersections =
      HashMap.intersectionWith (+) (countSteps wire1) (countSteps wire2)
  case part of
    Part1 -> print $ minimum $ manhattan <$> HashMap.keys intersections
    Part2 -> print $ getSum $ minimum $ HashMap.elems intersections

data Step = U | D | L | R
  deriving (Eq, Show)

data State = State
  { _pos :: Vec2 Int
  , _steps :: Sum Int
  , _points :: HashMap (Vec2 Int) (Sum Int)
  }

pos :: Lens' State (Vec2 Int)
pos = lens _pos $ \s v -> s { _pos = v }

steps :: Lens' State (Sum Int)
steps = lens _steps $ \s v -> s { _steps = v }

points :: Lens' State (HashMap (Vec2 Int) (Sum Int))
points = lens _points $ \s v -> s { _points = v }

countSteps :: [Step] -> HashMap (Vec2 Int) (Sum Int)
countSteps ps = foldl' step (State 0 1 HashMap.empty) ps ^. points
 where
  step s = \case
    U -> stepBy (Vec2.y +~ 1) s
    D -> stepBy (Vec2.y -~ 1) s
    L -> stepBy (Vec2.x -~ 1) s
    R -> stepBy (Vec2.x +~ 1) s

-- brittany-disable-next-binding

stepBy :: (Vec2 Int -> Vec2 Int) -> State -> State
stepBy f s = s
  & pos .~ newPos
  & steps %~ (+ 1)
  & points %~ HashMap.insertWith previous newPos (s ^. steps)
 where
  newPos = f $ s ^. pos
  previous _ v = v

parseThrows :: Parser a -> Text -> IO a
parseThrows parser =
  either (throwIO . userError) pure . parseOnly (parser <* endOfInput)

-- brittany-disable-next-binding

parseWire :: Parser [Step]
parseWire =
  concat <$> commaSep parseSteps
 where
   commaSep = (`sepBy1` char ',')

-- brittany-disable-next-binding

parseSteps :: Parser [Step]
parseSteps = do
  step <- asum
    [ U <$ char 'U'
    , D <$ char 'D'
    , L <$ char 'L'
    , R <$ char 'R'
    ]
  n <- decimal
  pure $ replicate n step

manhattan :: Vec2 Int -> Int
manhattan p = p ^. Vec2.x + p ^. Vec2.y
