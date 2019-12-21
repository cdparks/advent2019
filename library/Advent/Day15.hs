module Advent.Day15
  ( main
  )
where

import Advent.Prelude hiding (each, head, last, next)

import Advent.Heading (Heading(..))
import qualified Advent.Heading as Heading
import Advent.IntCode (run)
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Program)
import qualified Advent.IntCode.Program as Program
import Advent.Vec2 (Vec2(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (head, last)
import Data.Text.IO (getContents)
import Lens.Micro hiding (each)

main :: Part -> IO ()
main part = do
  program <- Program.parse <$> getContents
  let world = explore program
  case part of
    Part1 -> print $ shortestDistance world
    Part2 -> print $ timeToFill world

explore :: Program -> World
explore program = last worlds
 where
  output = unOutput $ snd $ run (Input $ fromHeading <$> input) program
  ~(input, worlds) = fromWorlds $ react $ toEnum <$> output

data Status
  = Hit
  | Moved
  | Found
  deriving (Eq, Show, Enum)

fromHeading :: Heading -> Int
fromHeading = \case
  North -> 1
  South -> 2
  East -> 3
  West -> 4

data Entity
  = Start
  | Empty
  | Wall
  | Goal
  deriving (Eq, Show)

data World = World
  { _space :: HashMap (Vec2 Int) Entity
  , _pos :: Vec2 Int
  , _heading :: Heading
  }

space :: Lens' World (HashMap (Vec2 Int) Entity)
space = lens _space $ \w s -> w { _space = s }

pos :: Lens' World (Vec2 Int)
pos = lens _pos $ \w p -> w { _pos = p }

heading :: Lens' World Heading
heading = lens _heading $ \w h -> w { _heading = h }

-- brittany-disable-next-binding

world0 :: World
world0 =  World
  { _space = HashMap.singleton 0 Start
  , _pos = 0
  , _heading = North
  }

fromWorlds :: [World] -> ([Heading], [World])
fromWorlds worlds = unzip $ do
  world <- worlds
  pure (world ^. heading, world)

-- brittany-disable-next-binding

react :: [Status] -> [World]
react = (world0 :) . loop world0
 where
   loop current = \case
     [] -> []
     Hit:signals -> do
       let
         next = current
           & space %~ (HashMap.insert (Heading.step (current ^. heading) (current ^. pos)) Wall)
           & heading %~ Heading.left
       next : loop next signals
     signal:signals -> do
       let
         entity
           | signal == Found = Goal
           | otherwise = Empty
         nextPos = Heading.step (current ^. heading) (current ^. pos)
         next = current
           & space %~ (HashMap.insertWith keepStart nextPos entity)
           & pos .~ nextPos
           & heading %~ Heading.right
       if next ^. pos == 0
         then [next]
         else next : loop next signals

   keepStart lhs = \case
     Start -> Start
     _ -> lhs

shortestDistance :: World -> Int
shortestDistance world =
  search (world ^. space) (pick Start world) (pick Goal world)

-- brittany-disable-next-binding

search :: HashMap (Vec2 Int) Entity -> Vec2 Int -> Vec2 Int -> Int
search area start goal = loop HashSet.empty [(start, 0)]
 where
  loop seen = \case
    [] -> maxBound
    (point, !distance) : points
      | point == goal -> distance
      | point `HashSet.member` seen -> loop seen points
      | otherwise -> do
        let queue = points <> children point (distance + 1)
        loop (HashSet.insert point seen) queue

  children point distance = do
    next <- (`Heading.step` point) <$> [North ..]
    entity <- maybeToList $ HashMap.lookup next area
    guard $ entity /= Wall
    pure (next, distance)

timeToFill :: World -> Int
timeToFill world = fill (pick Goal world) remaining
  where remaining = HashSet.fromList $ each (/= Wall) world

-- brittany-disable-next-binding

fill :: Vec2 Int -> HashSet (Vec2 Int) -> Int
fill start = subtract 1 . flip (loop 0) [start]
 where
  loop !steps remaining0 = \case
    [] -> steps
    points
      | HashSet.size remaining0 == 0 -> steps
      | otherwise -> do
        let remaining = foldr HashSet.delete remaining0 points
        loop (steps + 1) remaining $ children remaining points

  children remaining points = do
    point <- points
    next <- (`Heading.step` point) <$> [North ..]
    guard $ next `HashSet.member` remaining
    pure next

pick :: Entity -> World -> Vec2 Int
pick entity = head . each (== entity)

each :: (Entity -> Bool) -> World -> [Vec2 Int]
each p world = do
  (point, entity) <- HashMap.toList $ world ^. space
  guard $ p entity
  pure point
