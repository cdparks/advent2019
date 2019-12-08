module Advent.IntCode.Memory
  ( Memory(..)
  , fromList
  , (!)
  , HasMemory(..)
  , fetch
  , store
  , eom
  )
where

import Advent.Prelude hiding (fromList)

import Advent.IntCode.Address (Address)
import qualified Advent.IntCode.Address as Address
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Lens.Micro (Lens')
import Lens.Micro.Mtl (use, (%=))

newtype Memory = Memory { unMemory :: IntMap Int }

infixl 9 !
(!) :: Memory -> Address -> Int
(!) (Memory memory) = (IntMap.!) memory . Address.asInt
{-# INLINE (!) #-}

fromList :: [Int] -> Memory
fromList = Memory . IntMap.fromList . zip [0 ..]

fetch :: (MonadState s m, HasMemory s) => Address -> m Int
fetch address = do
  Memory mem <- use memoryLens
  pure $ mem IntMap.! Address.asInt address

store :: (MonadState s m, HasMemory s) => Address -> Int -> m ()
store address value =
  memoryLens %= coerce (IntMap.insert (Address.asInt address) value)

eom :: (MonadState s m, HasMemory s) => m Address
eom = do
  Memory mem <- use memoryLens
  pure $ Address.from $ 1 + fst (IntMap.findMax mem)

class HasMemory s where
  memoryLens :: Lens' s Memory

instance HasMemory Memory where
  memoryLens = id
  {-# INLINE memoryLens #-}
