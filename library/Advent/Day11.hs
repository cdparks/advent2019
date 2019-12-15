module Advent.Day11
  ( main
  )
where

import Advent.Prelude hiding (Left, Right, State, empty, last, state)

import Advent.IntCode (run)
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Program)
import qualified Advent.IntCode.Program as Program
import Advent.Point (Point(..))
import qualified Advent.Point as Point
import Data.Foldable (maximum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (last)
import Data.List.Split (chunksOf)
import Data.Text.IO (getContents)
import Lens.Micro hiding (at)

main :: Part -> IO ()
main part = do
  program <- Program.parse <$> getContents
  case part of
    Part1 -> print $ HashMap.size $ _panel $ paint Black program
    Part2 -> render $ _panel $ paint White program

paint :: Color -> Program -> State
paint color program = last states
 where
  states = scan (empty color) output
  output = unOutput $ snd $ run input program
  input = Input $ fromEnum <$> color : (current <$> states)

render :: HashMap (Point Int) Color -> IO ()
render space = do
  traverse_ putStrLn $ chunksOf (mx + 1) $ do
    y <- [0 .. my]
    x <- [0 .. mx]
    pure $ toChar $ at (Point x y) space
 where
  Point mx my = bounds space
  toChar = \case
    White -> '█'
    Black -> ' '

bounds :: HashMap (Point Int) Color -> Point Int
bounds space = Point (maximum $ _x <$> painted) (maximum $ _y <$> painted)
  where painted = HashMap.keys $ HashMap.filter (== White) space

data State = State
  { _panel :: HashMap (Point Int) Color
  , _pos :: Point Int
  , _heading :: Heading
  }

data Heading = North | South | East | West

empty :: Color -> State
empty color = State (HashMap.singleton 0 color) 0 North

data Color = Black | White
  deriving (Eq, Enum)

data Turn = Left | Right
  deriving (Eq, Enum)

move :: Point Int -> Heading -> Turn -> (Point Int, Heading)
move point heading0 turn = (step heading point, heading)
  where heading = rotate heading0 turn

step :: Heading -> Point Int -> Point Int
step = \case
  North -> Point.y -~ 1
  South -> Point.y +~ 1
  East -> Point.x +~ 1
  West -> Point.x -~ 1

rotate :: Heading -> Turn -> Heading
rotate heading turn = case (heading, turn) of
  (North, Left) -> West
  (North, Right) -> East
  (South, Left) -> East
  (South, Right) -> West
  (East, Left) -> North
  (East, Right) -> South
  (West, Left) -> South
  (West, Right) -> North

update :: Color -> Turn -> State -> State
update color turn State {..} = State
  { _panel = HashMap.insert _pos color _panel
  , _pos = pos
  , _heading = heading
  }
  where (pos, heading) = move _pos _heading turn

scan :: State -> [Int] -> [State]
scan state0 (color : turn : input) = state : scan state input
  where state = update (toEnum color) (toEnum turn) state0
scan state0 _ = [state0]

current :: State -> Color
current State {..} = at _pos _panel

at :: Point Int -> HashMap (Point Int) Color -> Color
at point = HashMap.lookupDefault Black point
