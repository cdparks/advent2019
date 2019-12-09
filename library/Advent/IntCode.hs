{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Advent.IntCode
  ( run
  )
where


import Advent.Prelude hiding (next, runState)

import Advent.IntCode.Address (Address)
import qualified Advent.IntCode.Address as Address
import Advent.IntCode.Input (HasInput, Input(..))
import qualified Advent.IntCode.Input as Input
import Advent.IntCode.Instruction (Instruction(..), decode)
import Advent.IntCode.Machine (HasBase(..), HasPC(..))
import qualified Advent.IntCode.Machine as Machine
import Advent.IntCode.Memory (HasMemory, Memory)
import qualified Advent.IntCode.Memory as Memory
import Advent.IntCode.Mode (Mode(..))
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Program)
import Control.Monad.State.Lazy (runState)
import Lens.Micro
import Lens.Micro.Mtl (use, (+=), (.=))

run :: Input -> Program -> (Memory, Output)
run input program = (machine ^. Machine.memory, Output output)
  where ~(output, machine) = runState loop $ Machine.new input program

loop :: IsMachine s m => m [Int]
loop = do
  i <- decode <$> fetch Immediate
  case opcode i of
    1 -> arith (+) i *> loop
    2 -> arith (*) i *> loop
    3 -> read i *> loop
    4 -> (:) <$> write i <*> loop
    5 -> jump (/= 0) i *> loop
    6 -> jump (== 0) i *> loop
    7 -> cmp (<) i *> loop
    8 -> cmp (==) i *> loop
    9 -> adjustBase i *> loop
    99 -> pure []
    op -> error $ "Unknown op code " <> show op

arith :: IsMachine s m => (Int -> Int -> Int) -> Instruction -> m ()
arith f Instruction {..} = do
  x <- fetch mode1
  y <- fetch mode2
  store mode3 $ f x y

cmp :: IsMachine s m => (Int -> Int -> Bool) -> Instruction -> m ()
cmp f = arith $ \x y -> bool 0 1 $ f x y

jump :: IsMachine s m => (Int -> Bool) -> Instruction -> m ()
jump f Instruction {..} = do
  c <- fetch mode1
  t <- fetch mode2
  when (f c) $ pcLens .= Address.from t

read :: IsMachine s m => Instruction -> m ()
read Instruction {..} = store mode1 =<< Input.read

write :: IsMachine s m => Instruction -> m Int
write Instruction {..} = fetch mode1

adjustBase :: IsMachine s m => Instruction -> m ()
adjustBase Instruction {..} = do
  offset <- fetch mode1
  baseLens += Address.from offset

fetch :: IsMachine s m => Mode -> m Int
fetch mode = do
  addr <- next
  case mode of
    Immediate -> pure $ Address.asInt addr
    Absolute -> Memory.fetch addr
    Relative -> do
      base <- use baseLens
      Memory.fetch $ base + addr

store :: IsMachine s m => Mode -> Int -> m ()
store mode value = do
  addr <- next
  case mode of
    Immediate -> error "Cannot store to immediate parameter"
    Absolute -> Memory.store addr value
    Relative -> do
      base <- use baseLens
      Memory.store (base + addr) value

next :: IsMachine s m => m Address
next = do
  pc <- use pcLens
  addr <- Memory.fetch pc
  pcLens += 1
  pure $ Address.from addr

type IsMachine s m
  = (MonadState s m, HasInput s, HasMemory s, HasBase s, HasPC s)
