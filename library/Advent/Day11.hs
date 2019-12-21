module Advent.Day11
  ( main
  )
where

import Advent.Prelude hiding (Left, Right, State, empty, last, state)

import Advent.Heading (Heading(..))
import qualified Advent.Heading as Heading
import Advent.IntCode (run)
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Program)
import qualified Advent.IntCode.Program as Program
import Advent.Vec2 (Vec2(..))
import Data.Foldable (maximum, minimum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (last)
import Data.List.Split (chunksOf)
import Data.Text.IO (getContents)

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

render :: HashMap (Vec2 Int) Color -> IO ()
render space = do
  traverse_ putStrLn $ chunksOf (mx + 1) $ do
    y <- [0, negate 1 .. my]
    x <- [0 .. mx]
    pure $ toChar $ at (Vec2 x y) space
 where
  Vec2 mx my = bounds space
  toChar = \case
    White -> '█'
    Black -> ' '

bounds :: HashMap (Vec2 Int) Color -> Vec2 Int
bounds space = Vec2 (maximum $ _x <$> painted) (minimum $ _y <$> painted)
  where painted = HashMap.keys $ HashMap.filter (== White) space

data State = State
  { _panel :: HashMap (Vec2 Int) Color
  , _pos :: Vec2 Int
  , _heading :: Heading
  }

empty :: Color -> State
empty color = State (HashMap.singleton 0 color) 0 North

data Color = Black | White
  deriving (Eq, Show, Enum)

data Turn = Left | Right
  deriving (Eq, Enum)

move :: Vec2 Int -> Heading -> Turn -> (Vec2 Int, Heading)
move point heading0 turn = (Heading.step heading point, heading)
 where
  heading = case turn of
    Left -> Heading.left heading0
    Right -> Heading.right heading0

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

at :: Vec2 Int -> HashMap (Vec2 Int) Color -> Color
at point = HashMap.lookupDefault Black point
