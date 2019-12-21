module Advent.Day17
  ( main
  )
where

import Advent.Prelude hiding (last, lines)

import Advent.Heading (Heading(..))
import qualified Advent.Heading as Heading
import Advent.IntCode (run)
import qualified Advent.IntCode.Address as Address
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Patch(..), Program)
import qualified Advent.IntCode.Program as Program
import Advent.Vec2 (Vec2(..))
import Data.Char (chr, ord)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (last, lines)
import Data.Text.IO (getContents)

main :: Part -> IO ()
main part = do
  program <- Program.parse <$> getContents
  case part of
    Part1 -> do
      let
        output = runBot (Patch 1) "" program
        space = toSpace $ lines $ chr <$> output
      print $ sum [ x * y | Vec2 x y <- findIntersections space ]
    Part2 -> print $ last $ runBot (Patch 2) movement program

runBot :: Patch -> String -> Program -> [Int]
runBot patch input =
  unOutput . snd . run (Input $ ord <$> input) . Program.patch
    [(Address.from 0, patch)]

data Entity
  = Empty
  | Wall
  | Bot Heading
  deriving (Eq, Show)

toSpace :: [String] -> HashMap (Vec2 Int) Entity
toSpace = HashMap.fromList . concatMap (uncurry step) . zip [0 ..]
 where
  step y line = do
    (x, c) <- zip [0 ..] line
    pure (Vec2 x y, toEntity c)

findIntersections :: HashMap (Vec2 Int) Entity -> [Vec2 Int]
findIntersections space = do
  (point, entity) <- HashMap.toList space
  guard $ entity == Wall
  guard $ neighbor North point
  guard $ neighbor South point
  guard $ neighbor East point
  guard $ neighbor West point
  pure point
 where
  neighbor heading point =
    HashMap.lookup (Heading.step heading point) space == Just Wall

toEntity :: Char -> Entity
toEntity = \case
  '#' -> Wall
  '^' -> Bot North
  'v' -> Bot South
  '>' -> Bot East
  '<' -> Bot West
  _ -> Empty

movement :: String
movement = mconcat
  [ "A,B,A,C,B,C,B,A,C,B\n"
  , "L,6,R,8,R,12,L,6,L,8\n"
  , "L,10,L,8,R,12\n"
  , "L,8,L,10,L,6,L,6\n"
  , "n\n"
  ]

{-

The movement above just hardcodes a path from the robot's initial
position (the ^ below) all the way to the end of the scaffold.
Manually picking out the pattern was easier than writing a program
to do it :|

........................................#########......
........................................#.......#......
........................................#.......#......
........................................#.......#......
........................................#.......#......
........................................#.......#......
....................................#############......
....................................#...#..............
....................................#...#..............
....................................#...#..............
....................................#...#########......
....................................#...........#......
..............................#########.........#......
..............................#.....#.#.........#......
............#############.....#.....######^.....#......
............#.................#.......#.........#......
..#######...#.............###########.#.........#......
..#.....#...#.............#...#.....#.#.........#......
#######.#...#.............#...#.....#.#.........#......
#.#...#.#...#.............#...#.....#.#.........#......
#.#...#.#...#.............#...#.....#.#.........#......
#.#...#.#...#.............#...#.....#.#.........#......
#.###########.............#...#######.#.........#######
#.....#.#.................#...........#...............#
#.....#.#.....#############...........#########.......#
#.....#.#.....#...............................#.......#
#########.....#...............................#.......#
......#.......#...............................#.......#
......#.......#.........................###########...#
......#.......#.........................#.....#...#...#
......#########.........................#.#############
........................................#.#...#...#....
........................................#.#...#...#....
........................................#.#...#...#....
........................................#######...#....
..........................................#.......#....
..........................................#########....
-}
