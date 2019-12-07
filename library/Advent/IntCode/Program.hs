module Advent.IntCode.Program
  ( Program
  , parse
  , instructions
  , patch
  , Patch(..)
  )
where

import Advent.Prelude

import Advent.IntCode.Address (Address)
import qualified Advent.IntCode.Address as Address
import Advent.Text
import Control.Monad.ST (ST)
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as M

newtype Program = Program { unProgram :: Vector Int }

parse :: Text -> Program
parse = Program . fromList . readCommaSep

instructions :: Program -> Vector Int
instructions = unProgram
{-# INLINE instructions #-}

data Patch
  = Noun Int
  | Verb Int

patch :: [(Address, Patch)] -> Program -> Program
patch patches = Program . U.modify preprocess . unProgram
 where
  preprocess :: forall s . MVector s Int -> ST s ()
  preprocess is = for_ patches $ \(addr, p) ->
    M.unsafeWrite is (Address.asInt addr) $ fromPatch p
  fromPatch = \case
    Noun i -> i
    Verb i -> i
