{-# LANGUAGE StrictData #-}

module Advent.IntCode.Decode
  ( decode
  , Instruction(..)
  , Addr(..)
  , Param(..)
  )
where

import Advent.Prelude hiding (State, next)

newtype Addr = Addr Int
  deriving Show

data Param
  = Imm Int
  | Pos Addr
  deriving Show

data Instruction
  = Add Param Param Param
  | Mul Param Param Param
  | Read Param
  | Write Param
  | Halt
  deriving Show

decode :: MonadFail m => [Int] -> m [Instruction]
decode is = reverse . instructions <$> execStateT loop (State is [])
 where
  loop = do
    mi <- next
    for_ mi $ \i -> do
      let RawInstruction {..} = toRaw i
      case opcode of
        1 -> do
          a <- param
          b <- param
          c <- param
          push $ Add (modeA a) (modeB b) (modeC c)
        2 -> do
          a <- param
          b <- param
          c <- param
          push $ Mul (modeA a) (modeB b) (modeC c)
        3 -> do
          a <- param
          push $ Read $ modeA a
        4 -> do
          a <- param
          push $ Write $ modeA a
        99 -> push Halt
        _ -> fail $ "Unrecognized opcode " <> show opcode
      loop

data RawInstruction = RawInstruction
  { opcode :: Int
  , modeA :: Int -> Param
  , modeB :: Int -> Param
  , modeC :: Int -> Param
  }

toRaw :: Int -> RawInstruction
toRaw i = RawInstruction
  { opcode = i `mod` 100
  , modeA = mode a
  , modeB = mode b
  , modeC = mode c
  }
 where
  a = i `div` 100
  b = a `div` 10
  c = b `div` 10
  mode x
    | x `mod` 10 == 1 = Imm
    | otherwise = Pos . Addr

data State = State
  { stream :: [Int]
  , instructions :: [Instruction]
  }

next :: MonadState State m => m (Maybe Int)
next = gets stream >>= \case
  [] -> pure Nothing
  x : xs -> do
    modify $ \s -> s { stream = xs }
    pure $ Just x

param :: (MonadFail m, MonadState State m) => m Int
param = do
  mx <- next
  case mx of
    Nothing -> fail "Not enough params"
    Just x -> pure x

push :: MonadState State m => Instruction -> m ()
push i = modify $ \s -> s { instructions = i : instructions s }
