module Advent.Day13
  ( main
  )
where

import Advent.Prelude hiding (last)

import qualified Advent.Image as Image
import Advent.IntCode (run)
import qualified Advent.IntCode.Address as Address
import Advent.IntCode.Input (Input(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Patch(..), Program)
import qualified Advent.IntCode.Program as Program
import Advent.Vec2 (Vec2(..))
import qualified Advent.Vec2 as Vec2
import Codec.Picture
import qualified Data.HashMap.Strict as HashMap
import Data.List (last)
import Data.Text.IO (getContents)
import Lens.Micro
import System.Environment (lookupEnv)

main :: Part -> IO ()
main part = do
  program <- Program.parse <$> getContents
  case part of
    Part1 -> do
      let output = play (Quarters 1) (Input []) program
      print $ length [ () | Draw _ Block <- output ]
    Part2 -> do
      let
        output = play (Quarters 2) (Input input) program
        input = react output
      print $ last [ score | Score score <- output ]
      traverse_ (animate output) =<< lookupEnv "SAVE"

play :: Patch -> Input -> Program -> [Signal]
play quarters input = decode . unOutput . snd . run input . Program.patch
  [(Address.from 0, quarters)]

decode :: [Int] -> [Signal]
decode = \case
  x : y : t : os
    | x == (-1) && y == 0 -> Score t : decode os
    | otherwise -> Draw (Vec2 x y) (toEnum t) : decode os
  _ -> []

-- brittany-disable-next-binding

react :: [Signal] -> [Int]
react = loop 22
 where
  loop _ [] = []
  loop _ (Draw paddle Paddle : ss) = loop (paddle ^. Vec2.x) ss
  loop x (Draw ball Ball : ss)
    | ball ^. Vec2.x < x = negate 1 : loop x ss
    | ball ^. Vec2.x > x = 1 : loop x ss
    | otherwise = 0 : loop x ss
  loop x (_ : ss) = loop x ss

data Signal
  = Draw (Vec2 Int) Tile
  | Score Int
  deriving (Eq, Show)

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Eq, Show, Enum)

animate :: [Signal] -> FilePath -> IO ()
animate signals filename = do
  putStrLn $ "Saving animation to " <> filename
  let frames = Image.scale 12 <$> generateFrames signals
  case writeGifAnimation filename 2 LoopingNever frames of
    Left err -> putStrLn err
    Right action -> do
      action
      putStrLn "Done"

-- brittany-disable-next-binding

generateFrames :: [Signal] -> [Image PixelRGB8]
generateFrames = dropFrames . zip [0..] . fmap toImage . paint HashMap.empty
 where
  paint !space = \case
    Draw p t : signals
      | t == Ball || t == Paddle -> share (HashMap.insert p t space) signals
      | otherwise -> paint (HashMap.insert p t space) signals
    _ : signals -> paint space signals
    [] -> [space]

  share !space signals = space : paint space signals
  toImage space = generateImage (shade space) width height
  shade space x y = toColor $ HashMap.lookupDefault Empty (Vec2 x y) space

  width = 44
  height = 20

dropFrames :: [(Int, a)] -> [a]
dropFrames = loop
 where
  loop = \case
    [] -> []
    (n, x) : xs -> x : loop (drop (upto n) xs)

  -- We'll drop up to 60 frames at a time in the middle of the
  -- animation, but at both ends we'll drop zero. The 7500 constant
  -- is carefully picked to make sure we don't drop the very last
  -- frame.
  upto :: Int -> Int
  upto n = round $ invert (interpolate n) * 60

  interpolate :: Int -> Double
  interpolate n = fromIntegral (abs $ n - 7500) / 7500

  invert :: Double -> Double
  invert x = abs $ 1 - x

toColor :: Tile -> PixelRGB8
toColor = \case
  Empty -> PixelRGB8 0 0 0
  Wall -> PixelRGB8 255 255 255
  Block -> PixelRGB8 0 255 0
  Paddle -> PixelRGB8 0 255 255
  Ball -> PixelRGB8 255 0 0
