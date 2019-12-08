module Advent.IntCode.Machine
  ( Machine
  , HasPC(..)
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
new input0 program =
  Machine { _pc = 0, _input = input0, _memory = Program.asMemory program }

data Machine = Machine
  { _pc :: Address
  , _input :: Input
  , _memory :: Memory
  }

pc :: Lens' Machine Address
pc = lens _pc $ \m x -> m { _pc = x }
{-# INLINE pc #-}

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

instance HasMemory Machine where
  memoryLens = memory
  {-# INLINE memoryLens #-}

instance HasInput Machine where
  inputLens = input
  {-# INLINE inputLens #-}
