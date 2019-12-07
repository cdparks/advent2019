module Advent.IntCode.Memory
  ( Memory(..)
  , (!)
  , RawMemory
  , new
  , fetch
  , store
  , freeze
  , eom
  )
where

import Advent.Prelude

import Advent.IntCode.Address (Address)
import qualified Advent.IntCode.Address as Address
import Control.Monad.ST (ST)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as M

newtype Memory = Memory { unMemory :: Vector Int }
newtype RawMemory s = RawMemory (MVector s Int)

infixl 9 !
(!) :: Memory -> Address -> Int
(!) (Memory memory) = (U.!) memory . Address.asInt
{-# INLINE (!) #-}

new :: Vector Int -> ST s (RawMemory s)
new = fmap RawMemory . U.thaw
{-# INLINE new #-}

fetch :: RawMemory s -> Address -> ST s Int
fetch (RawMemory mem) = M.unsafeRead mem . Address.asInt
{-# INLINE fetch #-}

store :: RawMemory s -> Address -> Int -> ST s ()
store (RawMemory mem) = M.unsafeWrite mem . Address.asInt
{-# INLINE store #-}

freeze :: RawMemory s -> ST s Memory
freeze (RawMemory mem) = Memory <$> U.freeze mem
{-# INLINE freeze #-}

eom :: RawMemory s -> Address
eom (RawMemory mem) = Address.from $ M.length mem
{-# INLINE eom #-}
