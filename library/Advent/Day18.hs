module Advent.Day18
  ( main
  )
where

import Advent.Prelude hiding (for, last)

import Advent.Heading (Heading(..))
import qualified Advent.Heading as Heading
import Advent.PriorityQueue (HasPriorityQueue(..))
import qualified Advent.PriorityQueue as PriorityQueue
import Advent.Queue (HasQueue(..))
import qualified Advent.Queue as Queue
import Advent.Vec2 (Vec2(..))
import qualified Advent.Vec2 as Vec2
import Data.Char (isLower, isUpper, toLower)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (last, maximum)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Semigroup (Arg(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (unpack)
import Data.Text.IO (getContents)
import Data.Traversable (for)
import Lens.Micro
import Lens.Micro.Mtl

main :: Part -> IO ()
main part = do
  space <- parse <$> getContents
  case part of
    Part1 -> print $ explore HashSet.empty [space]
    Part2 -> print $ explore (findKeys space) $ split space

data Entity
  = Empty
  | Key Char
  | Door Char
  | Entrance
  deriving (Eq, Show)

type Space = HashMap (Vec2 Int) Entity
type Graph = HashMap (Vec2 Int) (HashMap (Vec2 Int) Int)

explore :: HashSet Char -> [Space] -> Int
explore allKeys = sum . mapMaybe go
 where
  go space = steps
    (findEntrance space)
    (allKeys `HashSet.difference` findKeys space)
    space

parse :: Text -> Space
parse = HashMap.fromList . concatMap (uncurry step) . zip [0 ..] . lines
 where
  step y line = do
    (x, c) <- zip [0 ..] $ unpack line
    guard $ c /= '#'
    pure (Vec2 x y, toEntity c)

toGraph :: Space -> Graph
toGraph space = HashMap.fromList $ do
  (point, entity) <- HashMap.toList space
  guard $ isInteresting entity
  pure (point, findDistances entity point space)

data BFS = BFS
  { _distances :: HashMap (Vec2 Int) (Entity, Int)
  , _queue :: Seq (Vec2 Int)
  }

instance HasQueue BFS (Vec2 Int) where
  queueLens = lens _queue $ \b q -> b { _queue = q }

distances :: Lens' BFS (HashMap (Vec2 Int) (Entity, Int))
distances = lens _distances $ \b d -> b { _distances = d }

-- brittany-disable-next-binding

findDistances :: Entity -> Vec2 Int -> Space -> HashMap (Vec2 Int) Int
findDistances entity0 start space = HashMap.fromList $ evalState loop BFS
  { _distances = HashMap.singleton start (entity0, 0)
  , _queue = Seq.singleton start
  }
 where
  loop = do
    mCurrent <- Queue.pop
    case mCurrent of
      Nothing -> pure []
      Just point -> do
        distanceMap <- use distances
        let
          (entity, distance) = HashMap.lookupDefault (Empty, maxBound) point distanceMap
        if point /= start && isInteresting entity
          then ((point, distance) :) <$> loop
          else do
            let
              neighbors = do
                heading <- [North ..]
                let newPoint = Heading.step heading point
                guard $ not $ newPoint `HashMap.member` distanceMap
                neighbor <- maybeToList $ HashMap.lookup newPoint space
                pure (newPoint, (neighbor, distance + 1))

            Queue.extend $ Seq.fromList $ fst <$> neighbors
            update distances neighbors
            loop

data Dijkstra = Dijkstra
  { _priorityQueue :: MinQueue (Arg Int (Vec2 Int, HashSet Char))
  , _visited :: HashMap (Vec2 Int, HashSet Char) Int
  }

instance HasPriorityQueue Dijkstra (Arg Int (Vec2 Int, HashSet Char)) where
  priorityQueueLens = lens _priorityQueue $ \s q -> s { _priorityQueue = q }

visited :: Lens' Dijkstra (HashMap (Vec2 Int, HashSet Char) Int)
visited = lens _visited $ \s v -> s { _visited = v }

-- brittany-disable-next-binding

steps
  :: Vec2 Int
  -> HashSet Char
  -> Space
  -> Maybe Int
steps start initialKeys space = evalState loop Dijkstra
  { _priorityQueue = MinQueue.singleton (Arg 0 (start, initialKeys))
  , _visited = HashMap.singleton (start, initialKeys) 0
  }
 where
  graph = toGraph space
  keyMap = HashMap.mapMaybe fromKey space
  allKeys = HashSet.fromList $ HashMap.elems keyMap
  loop = do
    mCurrent <- PriorityQueue.pop
    fmap join . for mCurrent $ \(Arg distance (point, keys)) -> do
      let
        newKeys = fromMaybe keys $ do
          key <- HashMap.lookup point keyMap
          pure $ HashSet.insert key keys
      if all (`HashSet.member` newKeys) allKeys
        then pure $ Just distance
        else do
          seen <- use visited
          let
            neighbors = do
              (newPoint, weight) <- HashMap.toList $ graph ! point
              neighbor <- maybeToList $ HashMap.lookup newPoint space
              guard $ unlocked newKeys neighbor
              let
                prevDistance = HashMap.lookupDefault maxBound (newPoint, newKeys) seen
                newDistance = distance + weight
              guard $ newDistance < prevDistance
              pure (newDistance, (newPoint, newKeys))

          PriorityQueue.extend $ MinQueue.fromList $ uncurry Arg <$> neighbors
          update visited $ swap <$> neighbors
          loop

unlocked :: HashSet Char -> Entity -> Bool
unlocked keys = \case
  Door key -> toLower key `HashSet.member` keys
  _ -> True

fromKey :: Entity -> Maybe Char
fromKey = \case
  Key key -> pure key
  _ -> Nothing

isInteresting :: Entity -> Bool
isInteresting = (/= Empty)

toEntity :: Char -> Entity
toEntity = \case
  '@' -> Entrance
  char
    | isUpper char -> Door char
    | isLower char -> Key char
    | otherwise -> Empty

update
  :: (MonadState s m, Eq k, Hashable k)
  => Lens' s (HashMap k v)
  -> [(k, v)]
  -> m ()
update len items =
  for_ items $ \(key, value) -> len %= HashMap.insert key value

split :: HashMap (Vec2 Int) Entity -> [HashMap (Vec2 Int) Entity]
split space =
  [ filterSpace q1Origin q1Corner updated
  , filterSpace q2Origin q2Corner updated
  , filterSpace q3Origin q3Corner updated
  , filterSpace q4Origin q4Corner updated
  ]
 where
  Vec2 x y = findEntrance space
  Vec2 mx my = bounds space
  updated = foldr
    (`HashMap.insert` Entrance)
    space
    [ Vec2 (x + 1) (y + 1)
    , Vec2 (x + 1) (y - 1)
    , Vec2 (x - 1) (y + 1)
    , Vec2 (x - 1) (y - 1)
    ]
  q1Origin = Vec2 0 0
  q1Corner = Vec2 (x - 1) (y - 1)

  q2Origin = Vec2 (x + 1) 0
  q2Corner = Vec2 mx (y - 1)

  q3Origin = Vec2 0 (y + 1)
  q3Corner = Vec2 (x - 1) my

  q4Origin = Vec2 (x + 1) (y + 1)
  q4Corner = Vec2 mx my

bounds :: HashMap (Vec2 Int) Entity -> Vec2 Int
bounds space = Vec2 (maximum $ _x <$> points) (maximum $ _y <$> points)
  where points = HashMap.keys space

filterSpace
  :: Vec2 Int
  -> Vec2 Int
  -> HashMap (Vec2 Int) Entity
  -> HashMap (Vec2 Int) Entity
filterSpace origin corner = HashMap.mapMaybeWithKey $ \point entity -> do
  guard $ origin ^. Vec2.x <= point ^. Vec2.x
  guard $ origin ^. Vec2.y <= point ^. Vec2.y
  guard $ point ^. Vec2.x <= corner ^. Vec2.x
  guard $ point ^. Vec2.y <= corner ^. Vec2.y
  pure entity

findKeys :: HashMap (Vec2 Int) Entity -> HashSet Char
findKeys = HashSet.fromList . mapMaybe fromKey . HashMap.elems

findEntrance :: HashMap (Vec2 Int) Entity -> Vec2 Int
findEntrance space = last $ do
  (point, entity) <- HashMap.toList space
  guard $ entity == Entrance
  pure point
