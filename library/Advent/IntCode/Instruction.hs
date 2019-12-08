module Advent.IntCode.Instruction
  ( Instruction(..)
  , decode
  )
where

import Advent.Prelude

import Advent.IntCode.Mode (Mode(..))

data Instruction = Instruction
  { opcode :: Int
  , mode1 :: Mode
  , mode2 :: Mode
  , mode3 :: Mode -- currently unused
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
    | x `mod` 10 == 1 = Immediate
    | otherwise = Position
