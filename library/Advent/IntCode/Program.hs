module Advent.IntCode.Program
  ( Program
  , parse
  , asMemory
  , patch
  , Patch(..)
  )
where

import Advent.Prelude

import Advent.IntCode.Address (Address)
import Advent.IntCode.Memory (Memory)
import qualified Advent.IntCode.Memory as Memory
import Advent.Text
import Data.Text (Text)

newtype Program = Program { unProgram :: Memory }

parse :: Text -> Program
parse = Program . Memory.fromList . readCommaSep

asMemory :: Program -> Memory
asMemory = coerce
{-# INLINE asMemory #-}

data Patch
  = Noun Int
  | Verb Int

patch :: [(Address, Patch)] -> Program -> Program
patch patches = Program . execState preprocess . unProgram
 where
  preprocess = for_ patches $ \(addr, p) -> Memory.store addr $ fromPatch p
  fromPatch = \case
    Noun i -> i
    Verb i -> i
