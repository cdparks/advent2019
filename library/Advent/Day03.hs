{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day03
  ( main
  )
where

import Advent.Prelude hiding (State)

import Control.Exception (throwIO)
import Data.Attoparsec.Text hiding (D)
import Data.Foldable (minimum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)
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

data Point = Point
  { _x :: {-# UNPACK #-} Int
  , _y :: {-# UNPACK #-} Int
  }
  deriving (Eq, Show, Generic, Hashable)

x :: Lens' Point Int
x = lens _x $ \p v -> p { _x = v }

y :: Lens' Point Int
y = lens _y $ \p v -> p { _y = v }

data State = State
  { _pos :: {-# UNPACK #-} Point
  , _steps :: Sum Int
  , _points :: HashMap Point (Sum Int)
  }

pos :: Lens' State Point
pos = lens _pos $ \s v -> s { _pos = v }

steps :: Lens' State (Sum Int)
steps = lens _steps $ \s v -> s { _steps = v }

points :: Lens' State (HashMap Point (Sum Int))
points = lens _points $ \s v -> s { _points = v }

countSteps :: [Step] -> HashMap Point (Sum Int)
countSteps ps = foldl' step s0 ps ^. points
 where
  s0 = State (Point 0 0) 1 HashMap.empty
  step s = \case
    U -> stepBy (y %~ (+ 1)) s
    D -> stepBy (y %~ subtract 1) s
    L -> stepBy (x %~ subtract 1) s
    R -> stepBy (x %~ (+ 1)) s

-- brittany-disable-next-binding

stepBy :: (Point -> Point) -> State -> State
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

manhattan :: Point -> Int
manhattan p = p ^. x + p ^. y
