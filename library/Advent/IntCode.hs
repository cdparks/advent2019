{-# LANGUAGE TupleSections #-}

module Advent.IntCode
  ( run
  )
where


import Advent.Prelude hiding (runState)

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
  where ~(output, machine) = runState eval $ Machine.new input program

eval
  :: forall s m
   . (MonadState s m, HasPC s, HasBase s, HasInput s, HasMemory s)
  => m [Int]
eval = decoded $ \i -> case opcode i of
  1 -> arith (+) i
  2 -> arith (*) i
  3 -> read i
  4 -> write i
  5 -> jump (/= 0) i
  6 -> jump (== 0) i
  7 -> cmp (<) i
  8 -> cmp (==) i
  9 -> adjustBase i
  99 -> pure []
  op -> error $ "Unknown op code " <> show op
 where
  decoded :: (Instruction -> m [Int]) -> m [Int]
  decoded body = do
    pc <- use pcLens
    addr <- Memory.eom
    if pc < addr then body =<< decode <$> fetch Immediate else pure []

  arith :: (Int -> Int -> Int) -> Instruction -> m [Int]
  arith f Instruction {..} = do
    x <- fetch mode1
    y <- fetch mode2
    store mode3 $ f x y
    eval

  cmp :: (Int -> Int -> Bool) -> Instruction -> m [Int]
  cmp f = arith $ \x y -> bool 0 1 $ f x y

  jump :: (Int -> Bool) -> Instruction -> m [Int]
  jump f Instruction {..} = do
    c <- fetch mode1
    t <- fetch mode2
    when (f c) $ pcLens .= Address.from t
    eval

  read :: Instruction -> m [Int]
  read Instruction{..} = do
    i <- Input.read
    store mode1 i
    eval

  write :: Instruction -> m [Int]
  write Instruction {..} = do
    x <- fetch mode1
    (x :) <$> eval

  adjustBase :: Instruction -> m [Int]
  adjustBase Instruction {..} = do
    offset <- fetch mode1
    baseLens += Address.from offset
    eval

  fetch :: Mode -> m Int
  fetch mode = do
    pc <- use pcLens
    addr <- Memory.fetch pc
    pcLens += 1
    case mode of
      Immediate -> pure addr
      Absolute -> Memory.fetch $ Address.from addr
      Relative -> do
        base <- use baseLens
        Memory.fetch $ base + Address.from addr

  store :: Mode -> Int -> m ()
  store mode value = do
    pc <- use pcLens
    addr <- Memory.fetch pc
    pcLens += 1
    case mode of
      Immediate -> error "Cannot store to immediate parameter"
      Absolute -> Memory.store (Address.from addr) value
      Relative -> do
        base <- use baseLens
        Memory.store (base + Address.from addr) value
