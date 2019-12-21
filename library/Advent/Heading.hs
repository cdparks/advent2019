module Advent.Heading
  ( Heading(..)
  , right
  , left
  , move
  , step
  )
where

import Advent.Prelude

import Advent.Vec2 (Vec2)
import qualified Advent.Vec2 as Vec2
import Lens.Micro

data Heading
  = North
  | South
  | East
  | West
  deriving (Eq, Show, Enum)

left :: Heading -> Heading
left = \case
  North -> West
  South -> East
  East -> North
  West -> South

right :: Heading -> Heading
right = \case
  North -> East
  South -> West
  East -> South
  West -> North

step :: Heading -> Vec2 Int -> Vec2 Int
step = move 1

move :: Int -> Heading -> Vec2 Int -> Vec2 Int
move steps = \case
  North -> Vec2.y +~ steps
  South -> Vec2.y -~ steps
  East -> Vec2.x +~ steps
  West -> Vec2.x -~ steps
