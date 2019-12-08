{-# LANGUAGE StrictData #-}
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
import qualified Advent.IntCode.Machine as Machine
import Advent.IntCode.Memory (HasMemory, Memory)
import qualified Advent.IntCode.Memory as Memory
import Advent.IntCode.Output (Output(..))
import Advent.IntCode.Program (Program)
import Control.Monad.State.Lazy (runState)
import Lens.Micro

run :: Input -> Program -> (Memory, Output)
run input program = (machine ^. Machine.memory, Output output)
 where
  ~(output, machine) = runState evaluate $ Machine.new input program

evaluate :: forall  s m . (MonadState s m, HasInput s, HasMemory s) => m [Int]
evaluate = loop 0
 where
  loop :: Address -> m [Int]
  loop pc = do
    i <- decode <$> Memory.fetch pc
    case opcode i of
      1 -> arith (+) i pc
      2 -> arith (*) i pc
      3 -> read i pc
      4 -> write i pc
      5 -> jump (/= 0) i pc
      6 -> jump (== 0) i pc
      7 -> cmp (<) i pc
      8 -> cmp (==) i pc
      99 -> pure []
      op -> error $ "Unknown op code " <> show op

  next :: Address -> m [Int]
  next pc = do
    addr <- Memory.eom
    if pc < addr then loop pc else pure []

  arith :: (Int -> Int -> Int) -> Instruction -> Address -> m [Int]
  arith f Instruction {..} pc = do
    x <- fetch $ mode1 $ pc + 1
    y <- fetch $ mode2 $ pc + 2
    store (pc + 3) $ f x y
    next $ pc + 4

  cmp :: (Int -> Int -> Bool) -> Instruction -> Address -> m [Int]
  cmp f = arith $ \x y -> bool 0 1 $ f x y

  jump :: (Int -> Bool) -> Instruction -> Address -> m [Int]
  jump f Instruction {..} pc = do
    c <- fetch $ mode1 $ pc + 1
    t <- fetch $ mode2 $ pc + 2
    if f c then next $ Address.from t else next $ pc + 3

  read :: Instruction -> Address -> m [Int]
  read Instruction{} pc = do
    i <- Input.read
    store (pc + 1) i
    next $ pc + 2

  write :: Instruction -> Address -> m [Int]
  write Instruction {..} pc = do
    x <- fetch $ mode1 $ pc + 1
    (x :) <$> next (pc + 2)

  fetch :: Param -> m Int
  fetch = \case
    Imm addr -> Memory.fetch addr
    Pos addr -> do
      pos <- Memory.fetch addr
      Memory.fetch $ Address.from pos

  store :: Address -> Int -> m ()
  store addr value = do
    pos <- Memory.fetch addr
    Memory.store (Address.from pos) value

data Param
  = Imm Address
  | Pos Address
  deriving Show

data Instruction = Instruction
  { opcode :: Int
  , mode1 :: Address -> Param
  , mode2 :: Address -> Param
  , mode3 :: Address -> Param -- currently unused
  }

decode :: Int -> Instruction
decode i = Instruction
  { opcode = i `mod` 100
  , mode1 = mode a
  , mode2 = mode b
  , mode3 = mode c
  }
 where
  a = i `div` 100
  b = a `div` 10
  c = b `div` 10
  mode x
    | x `mod` 10 == 1 = Imm
    | otherwise = Pos
