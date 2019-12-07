module Advent.Day06
  ( main
  )
where

import Advent.Prelude

import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Text (split)
import Data.Text.IO (getContents)

main :: IO ()
main = do
  graph <- parse <$> getContents
  let
    inverted = invert graph
    santaParents = parents inverted "SAN"
    yourParents = parents inverted "YOU"
    pathUp = yourParents `pathUntil` santaParents
    pathDown = santaParents `pathUntil` yourParents

  print $ length $ pathUp <> reverse pathDown

type Tree = Node Label
type Label = Text

data Node a = Node
  { _label :: a
  , _children :: [Node a]
  }
  deriving (Functor, Foldable)

type Graph = HashMap Label [Label]
newtype Inverted = Inverted Graph

tree :: Graph -> Label -> Tree
tree graph = walk
 where
  walk label = Node label $ do
    children <- maybeToList $ HashMap.lookup label graph
    walk <$> children

invert :: Graph -> Inverted
invert graph = Inverted $ HashMap.fromListWith (<>) $ do
  (k, vs) <- HashMap.toList graph
  v <- vs
  [(v, [k]), (k, [])]

parents :: Inverted -> Label -> [Label]
parents (Inverted graph) = concatMap toList . _children . tree graph

pathUntil :: [Label] -> [Label] -> [Label]
pathUntil path other = takeWhile (not . (`HashSet.member` otherSet)) path
  where otherSet = HashSet.fromList other

_orbits :: Tree -> Int
_orbits = walk 0 where walk n node = n + sum (walk (n + 1) <$> _children node)

parse :: Text -> Graph
parse text = HashMap.fromListWith (<>) $ do
  line <- lines text
  case split (== ')') line of
    [k, v] -> [(k, [v]), (v, [])]
    _ -> []
