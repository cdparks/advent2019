module Advent.IntCode
  ( parse
  , run
  , at
  , Noun(..)
  , Verb(..)
  , Program
  , Memory
  )
where


import Advent.Prelude hiding (modify, next)

import Control.Monad.ST (ST)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed (modify, (!))
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V

newtype Noun = Noun Int
newtype Verb = Verb Int

newtype Program = Program (Vector Int)
newtype Memory = Memory (Vector Int)

infixl 9 `at`
at :: Memory -> Int -> Int
at (Memory memory) address = memory ! address

parse :: Text -> Program
parse = Program . fromList . mapMaybe (readMaybe . T.unpack) . T.splitOn ","

run :: Noun -> Verb -> Program -> Memory
run noun verb (Program instructions) =
  Memory $ modify (evaluate noun verb) instructions

evaluate :: forall s . Noun -> Verb -> MVector s Int -> ST s ()
evaluate (Noun noun) (Verb verb) instructions = do
  preprocess
  loop 0
 where
  loop :: Int -> ST s ()
  loop pc = do
    instruction <- V.unsafeRead instructions pc
    case instruction of
      1 -> binOp pc (+)
      2 -> binOp pc (*)
      99 -> pure ()
      _ -> error $ "Unknown instruction " <> show instruction

  next :: Int -> ST s ()
  next pc = when (pc < V.length instructions) $ loop pc

  preprocess :: ST s ()
  preprocess = do
    V.unsafeWrite instructions 1 noun
    V.unsafeWrite instructions 2 verb

  fetch :: Int -> ST s Int
  fetch addr = do
    position <- V.unsafeRead instructions addr
    V.unsafeRead instructions position

  store :: Int -> Int -> ST s ()
  store addr value = do
    position <- V.unsafeRead instructions addr
    V.unsafeWrite instructions position value

  binOp :: Int -> (Int -> Int -> Int) -> ST s ()
  binOp addr f = do
    x <- fetch $ addr + 1
    y <- fetch $ addr + 2
    store (addr + 3) $ f x y
    next $ addr + 4
