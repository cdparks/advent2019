{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

module Advent.Queue
  ( HasQueue(..)
  , pop
  , push
  , extend
  , drain
  )
where

import Advent.Prelude

import Data.Sequence (pattern (:<|), pattern (:|>), pattern Empty, Seq)
import Lens.Micro
import Lens.Micro.Mtl

class HasQueue s a | s -> a where
  queueLens :: Lens' s (Seq a)

instance HasQueue (Seq a) a where
  queueLens = id

pop :: (MonadState s m, HasQueue s a) => m (Maybe a)
pop = do
  queue <- use queueLens
  case queue of
    Empty -> pure Nothing
    a :<| as -> do
      queueLens .= as
      pure $ Just a

drain :: (MonadState s m, HasQueue s a) => (s -> r) -> (a -> m ()) -> m r
drain done work = loop
 where
  loop = pop >>= \case
    Nothing -> done <$> get
    Just a -> work a *> loop

push :: (MonadState s m, HasQueue s a) => a -> m ()
push a = queueLens %= (:|> a)

extend :: (MonadState s m, HasQueue s a) => Seq a -> m ()
extend queue = queueLens %= (<> queue)
