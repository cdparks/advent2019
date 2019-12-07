{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Advent.IntCode
  ( run
  )
where


import Advent.Prelude hiding (State, next)

import Advent.IntCode.Address (Address)
import qualified Advent.IntCode.Address as Address
import Advent.IntCode.Input (Input(..), RawInput)
import qualified Advent.IntCode.Input as Input
import Advent.IntCode.Memory (Memory, RawMemory)
import qualified Advent.IntCode.Memory as Memory
import Advent.IntCode.Output (Output(..), RawOutput)
import qualified Advent.IntCode.Output as Output
import Advent.IntCode.Program (Program)
import qualified Advent.IntCode.Program as Program
import Control.Monad.ST (ST, runST)

run :: Input -> Program -> (Memory, Output)
run input program = runST $ do
  stream <- Input.new input
  output <- Output.new
  memory <- Memory.new $ Program.instructions program
  evaluate stream output memory
  (,) <$> Memory.freeze memory <*> Output.freeze output

evaluate :: forall s . RawInput s -> RawOutput s -> RawMemory s -> ST s ()
evaluate input output mem = loop 0
 where
  loop :: Address -> ST s ()
  loop pc = do
    i <- decode <$> Memory.fetch mem pc
    case opcode i of
      1 -> arith (+) i pc
      2 -> arith (*) i pc
      3 -> read i pc
      4 -> write i pc
      5 -> jump (/= 0) i pc
      6 -> jump (== 0) i pc
      7 -> cmp (<) i pc
      8 -> cmp (==) i pc
      99 -> pure ()
      op -> error $ "Unknown op code " <> show op

  next :: Address -> ST s ()
  next pc = when (pc < Memory.eom mem) $ loop pc

  arith :: (Int -> Int -> Int) -> Instruction -> Address -> ST s ()
  arith f Instruction {..} pc = do
    x <- fetch $ mode1 $ pc + 1
    y <- fetch $ mode2 $ pc + 2
    store (pc + 3) $ f x y
    next $ pc + 4

  cmp :: (Int -> Int -> Bool) -> Instruction -> Address -> ST s ()
  cmp f = arith $ \x y -> bool 0 1 $ f x y

  jump :: (Int -> Bool) -> Instruction -> Address -> ST s ()
  jump f Instruction {..} pc = do
    c <- fetch $ mode1 $ pc + 1
    t <- fetch $ mode2 $ pc + 2
    if f c then next (Address.from t) else next $ pc + 3

  read :: Instruction -> Address -> ST s ()
  read Instruction{} pc = do
    i <- Input.read input
    store (pc + 1) i
    next $ pc + 2

  write :: Instruction -> Address -> ST s ()
  write Instruction {..} pc = do
    x <- fetch $ mode1 $ pc + 1
    Output.write output x
    next $ pc + 2

  fetch :: Param -> ST s Int
  fetch = \case
    Imm addr -> Memory.fetch mem addr
    Pos addr -> do
      pos <- Memory.fetch mem addr
      Memory.fetch mem $ Address.from pos

  store :: Address -> Int -> ST s ()
  store addr value = do
    pos <- Memory.fetch mem addr
    Memory.store mem (Address.from pos) value

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
