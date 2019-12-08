{-# LANGUAGE StrictData #-}

module Advent.Day04
  ( main
  )
where

import Advent.Prelude

import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V

main :: IO ()
main = print $ length $ filter (valid . pairs . digits) [178416 .. 676461]

-- Strict pair
data Pair = Pair {-# UNPACK #-} Int Int
  deriving Show

valid :: [Pair] -> Bool
valid ps0 = runST $ do
  dupes <- V.replicate 10 0
  loop dupes ps0
 where
  loop :: forall s . MVector s Int -> [Pair] -> ST s Bool
  loop dupes = \case
    [] -> U.any (== 1) <$> U.unsafeFreeze dupes
    Pair x y : ps
      | x < y -> loop dupes ps
      | x > y -> pure False
      | otherwise -> do
        V.unsafeModify dupes (+ 1) x
        loop dupes ps

pairs :: [Int] -> [Pair]
pairs xs = zipWith Pair xs $ drop 1 xs

digits :: Int -> [Int]
digits = fmap (subtract zero . ord) . show where zero = ord '0'