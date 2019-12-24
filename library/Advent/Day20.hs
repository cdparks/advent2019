{-# LANGUAGE TupleSections #-}

module Advent.Day20
  ( main
  )
where

import Advent.Prelude hiding (head)

import qualified Advent.Queue as Queue
import Advent.Vec2 (Vec2(..))
import qualified Advent.Vec2 as Vec2
import Data.Char (isUpper)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (head, lookup, maximum)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import Data.Text.IO (getContents)

main :: Part -> IO ()
main part = do
  space <- parse <$> getContents
  print $ search recurse (label "AA" space) (label "ZZ" space) space
 where
  recurse = case part of
    Part1 -> Recurse False
    Part2 -> Recurse True

search :: Recurse -> Vec2 Int -> Vec2 Int -> Space -> Int
search (Recurse recurse) start dest space =
  evalState (loop $ HashSet.singleton (0, start))
    $ Seq.singleton (0, (0, start))
 where
  portals = connectPortals space
  loop visited = Queue.pop >>= \case
    Nothing -> pure maxBound
    Just (steps, (level, point))
      | point == dest && level == 0 -> pure steps
      | otherwise -> do
        let
          empties = (level, ) <$> Vec2.neighbors point
          jumps = maybeToList $ do
            (loc, pos) <- HashMap.lookup point portals
            if recurse
              then do
                guard $ valid level loc
                pure (level + change loc, pos)
              else pure (level, pos)
          neighbors = do
            neighbor@(_, newPoint) <- jumps <> empties
            guard $ not $ neighbor `HashSet.member` visited
            guard $ newPoint `HashMap.member` space
            pure (steps + 1, neighbor)
        Queue.extend $ Seq.fromList neighbors
        loop $ visited <> HashSet.fromList (snd <$> neighbors)

data Entity
  = Empty
  | Portal Location Text
  deriving (Eq, Show)

data Location = Outer | Inner
  deriving (Eq, Show)

newtype Recurse = Recurse Bool

type Space = HashMap (Vec2 Int) Entity

parse :: Text -> Space
parse = uncurry groupChars . loop HashMap.empty [] . positioned
 where
  loop space chars = \case
    [] -> (space, reverse chars)
    (p, c) : cs
      | c == '.' -> loop (HashMap.insert p Empty space) chars cs
      | otherwise -> loop space ((p, c) : chars) cs

positioned :: Text -> [(Vec2 Int, Char)]
positioned = concatMap (uncurry step) . zip [0 ..] . lines
 where
  step y line = do
    (x, c) <- zip [0 ..] $ unpack line
    guard $ isUpper c || c == '.'
    pure (Vec2 x y, c)

groupChars :: Space -> [(Vec2 Int, Char)] -> Space
groupChars space = (<> space) . HashMap.fromList . loop
 where
  midpoint = findMidpoint space
  loop = \case
    [] -> []
    (p1, c1) : others -> do
      let
        (p2, c2) = head $ do
          neighbor <- Vec2.neighbors p1
          char <- maybeToList $ lookup neighbor others
          pure (neighbor, char)
        node = best midpoint (p1, c1) (p2, c2) space
      node : loop (filter ((/= p2) . fst) others)

best
  :: Vec2 Int
  -> (Vec2 Int, Char)
  -> (Vec2 Int, Char)
  -> Space
  -> (Vec2 Int, Entity)
best midpoint (p1, c1) (p2, c2) space = (node, entity)
 where
  node = head $ do
    neighbor <- Vec2.neighbors p1 <> Vec2.neighbors p2
    Empty <- maybeToList $ HashMap.lookup neighbor space
    pure neighbor
  entity = Portal location $ pack [c1, c2]
  location
    | p1 `distSquared` midpoint < node `distSquared` midpoint = Inner
    | otherwise = Outer

distSquared :: Vec2 Int -> Vec2 Int -> Int
distSquared (Vec2 x1 y1) (Vec2 x2 y2) = square (x2 - x1) + square (y2 - y1)
  where square x = x * x

findMidpoint :: Space -> Vec2 Int
findMidpoint space = Vec2 (mx `div` 2) (my `div` 2)
 where
  mx = maximum $ _x <$> points
  my = maximum $ _y <$> points
  points = HashMap.keys space

change :: Location -> Int
change = \case
  Inner -> 1
  Outer -> negate 1

valid :: Int -> Location -> Bool
valid 0 Outer = False
valid _ _ = True

connectPortals :: Space -> HashMap (Vec2 Int) (Location, Vec2 Int)
connectPortals space = HashMap.fromList $ do
  ends <- HashMap.elems portals
  case ends of
    [(loc1, p1), (loc2, p2)] -> [(p1, (loc1, p2)), (p2, (loc2, p1))]
    _ -> []
 where
  portals = HashMap.fromListWith (<>) $ do
    (pos, entity) <- HashMap.toList space
    Portal loc name <- pure entity
    pure (name, [(loc, pos)])

label :: Text -> Space -> Vec2 Int
label t space = head $ do
  (pos, Portal _ name) <- HashMap.toList space
  guard $ name == t
  pure pos
