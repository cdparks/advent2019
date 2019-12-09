module Advent.IntCode.Machine
  ( Machine
  , HasPC(..)
  , HasBase(..)
  , input
  , memory
  , new
  )
where

import Advent.Prelude

import Advent.IntCode.Address (Address)
import Advent.IntCode.Input (HasInput(..), Input)
import Advent.IntCode.Memory (HasMemory(..), Memory)
import Advent.IntCode.Program (Program)
import qualified Advent.IntCode.Program as Program
import Lens.Micro

new :: Input -> Program -> Machine
new input0 program = Machine
  { _pc = 0
  , _base = 0
  , _input = input0
  , _memory = Program.asMemory program
  }

data Machine = Machine
  { _pc :: Address
  , _base :: Address
  , _input :: Input
  , _memory :: Memory
  }

pc :: Lens' Machine Address
pc = lens _pc $ \m x -> m { _pc = x }
{-# INLINE pc #-}

base :: Lens' Machine Address
base = lens _base $ \m x -> m { _base = x }
{-# INLINE base #-}

input :: Lens' Machine Input
input = lens _input $ \m x -> m { _input = x }
{-# INLINE input #-}

memory :: Lens' Machine Memory
memory = lens _memory $ \m x -> m { _memory = x }
{-# INLINE memory #-}

class HasPC s where
  pcLens :: Lens' s Address

instance HasPC Machine where
  pcLens = pc
  {-# INLINE pcLens #-}

class HasBase s where
  baseLens :: Lens' s Address

instance HasBase Machine where
  baseLens = base
  {-# INLINE baseLens #-}

instance HasMemory Machine where
  memoryLens = memory
  {-# INLINE memoryLens #-}

instance HasInput Machine where
  inputLens = input
  {-# INLINE inputLens #-}
