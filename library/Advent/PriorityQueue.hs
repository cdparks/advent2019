{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

module Advent.PriorityQueue
  ( HasPriorityQueue(..)
  , pop
  , push
  , extend
  , drain
  )
where

import Advent.Prelude

import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Lens.Micro
import Lens.Micro.Mtl

class HasPriorityQueue s a | s -> a where
  priorityQueueLens :: Lens' s (MinQueue a)

instance HasPriorityQueue (MinQueue a) a where
  priorityQueueLens = id

pop :: (MonadState s m, HasPriorityQueue s a, Ord a) => m (Maybe a)
pop = do
  queue <- use priorityQueueLens
  case MinQueue.minView queue of
    Nothing -> pure Nothing
    Just (a, as) -> do
      priorityQueueLens .= as
      pure $ Just a

drain
  :: (MonadState s m, HasPriorityQueue s a, Ord a)
  => (s -> r)
  -> (a -> m ())
  -> m r
drain done work = loop
 where
  loop = pop >>= \case
    Nothing -> done <$> get
    Just a -> work a *> loop

push :: (MonadState s m, HasPriorityQueue s a, Ord a) => a -> m ()
push a = priorityQueueLens %= (MinQueue.insert a)

extend :: (MonadState s m, HasPriorityQueue s a, Ord a) => MinQueue a -> m ()
extend queue = priorityQueueLens %= (<> queue)
