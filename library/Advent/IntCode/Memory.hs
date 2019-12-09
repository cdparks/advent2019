module Advent.IntCode.Memory
  ( Memory(..)
  , fromList
  , (!)
  , HasMemory(..)
  , fetch
  , store
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
(!) (Memory memory) address = IntMap.findWithDefault 0 index memory
  where index = Address.asInt address
{-# INLINE (!) #-}

fromList :: [Int] -> Memory
fromList = Memory . IntMap.fromList . zip [0 ..]

fetch :: (MonadState s m, HasMemory s) => Address -> m Int
fetch address = (! address) <$> use memoryLens

store :: (MonadState s m, HasMemory s) => Address -> Int -> m ()
store address value =
  memoryLens %= coerce (IntMap.insert (Address.asInt address) value)

class HasMemory s where
  memoryLens :: Lens' s Memory

instance HasMemory Memory where
  memoryLens = id
  {-# INLINE memoryLens #-}
