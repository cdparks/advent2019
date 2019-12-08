{-# LANGUAGE StrictData #-}

module Advent.IntCode.Machine
  ( Machine
  , input
  , memory
  , new
  )
where

import Advent.Prelude

import Advent.IntCode.Input (HasInput(..), Input)
import Advent.IntCode.Memory (HasMemory(..), Memory)
import Advent.IntCode.Program (Program)
import qualified Advent.IntCode.Program as Program
import Lens.Micro

new :: Input -> Program -> Machine
new input0 program =
  Machine { _input = input0, _memory = Program.asMemory program }

data Machine = Machine
  { _input :: Input
  , _memory :: Memory
  }

input :: Lens' Machine Input
input = lens _input $ \m x -> m { _input = x }
{-# INLINE input #-}

memory :: Lens' Machine Memory
memory = lens _memory $ \m x -> m { _memory = x }
{-# INLINE memory #-}

instance HasMemory Machine where
  memoryLens = memory
  {-# INLINE memoryLens #-}

instance HasInput Machine where
  inputLens = input
  {-# INLINE inputLens #-}
