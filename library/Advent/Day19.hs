module Advent.Day19
  ( main
  )
where

import Advent.Prelude hiding (head)

import Advent.IntCode (run)
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Program)
import qualified Advent.IntCode.Program as Program
import Advent.Vec2 (Vec2(..))
import Data.List (head)
import Data.Text.IO (getContents)

main :: Part -> IO ()
main part = do
  program <- Program.parse <$> getContents
  case part of
    Part1 -> print $ length $ do
      y <- [0 .. 49]
      x <- [0 .. 49]
      guard $ inBeam program $ Vec2 x y
    Part2 -> do
      let Vec2 x y = findSquare program
      print $ x * 10000 + y

findSquare :: Program -> Vec2 Int
findSquare program = topRightCorner 0 0
 where
  topRightCorner !x !y
    | inBeam program (Vec2 (x + 99) y) = Vec2 x y
    | otherwise = bottomLeftCorner x (y + 1)
  bottomLeftCorner !x !y
    | inBeam program (Vec2 x (y + 99)) = topRightCorner x y
    | otherwise = bottomLeftCorner (x + 1) y

inBeam :: Program -> Vec2 Int -> Bool
inBeam program (Vec2 x y) = status == 1
  where status = head $ unOutput $ snd $ run (Input [x, y]) program
