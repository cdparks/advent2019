module Advent.Day16
  ( main
  )
where

import Advent.Prelude hiding (last)

import Advent.Text (fromDigit, toDigit)
import Data.Char (isDigit)
import Data.List (last, scanl')
import Data.Text (Text, unpack)
import Data.Text.IO (getContents)
import Text.Read (read)

main :: Part -> IO ()
main part = do
  digits <- toDigits <$> getContents
  case part of
    Part1 -> do
      let phase = step100 part1 digits
      putStrLn $ fromDigits $ take 8 phase
    Part2 -> do
      let phase = reversing (step100 part2) $ trim $ expand digits
      putStrLn $ fromDigits $ take 8 phase

part1 :: [Int] -> [Int]
part1 ds = map (flip digitAt ds) [1 .. length ds]

digitAt :: Int -> [Int] -> Int
digitAt n xs = abs (sum $ zipWith (*) xs (drop 1 $ cycle $ pattern n)) `mod` 10

pattern :: Int -> [Int]
pattern n = concatMap (replicate n) basePattern

basePattern :: [Int]
basePattern = [0, 1, 0, negate 1]

part2 :: [Int] -> [Int]
part2 = drop 1 . scanl' step 0 where step !x !y = (x + y) `mod` 10

expand :: [a] -> [a]
expand = concat . replicate 10000

trim :: [Int] -> [Int]
trim digits = drop offset digits
  where offset = read $ fromDigit <$> take 7 digits

step100 :: ([a] -> [a]) -> [a] -> [a]
step100 f = last . take 100 . iterate f . f

reversing :: ([a] -> [a]) -> [a] -> [a]
reversing f = reverse . f . reverse

toDigits :: Text -> [Int]
toDigits = map toDigit . filter isDigit . unpack

fromDigits :: [Int] -> String
fromDigits = map fromDigit
