{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day14
  ( main
  )
where

import Advent.Prelude hiding (yield)

import Control.Exception (throwIO)
import Data.Attoparsec.Text hiding (D)
import Data.Char (isUpper)
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (pattern (:<|), pattern Empty, Seq)
import qualified Data.Sequence as Seq
import Data.Text (pack)
import Data.Text.IO (getContents)
import Lens.Micro
import Lens.Micro.Mtl
import System.IO.Error (userError)

main :: Part -> IO ()
main part = do
  graph <- parseThrows parseGraph =<< getContents
  case part of
    Part1 -> print $ minimumOre graph 1
    Part2 -> print $ maximumFuel graph 1000000000000

-- brittany-disable-next-binding

minimumOre :: Graph -> Integer -> Integer
minimumOre graph want = evalState process World
    { _reactions = graph
    , _queue = Seq.singleton ("FUEL", want)
    , _extras = HashMap.empty
    , _ore = 0
    }

process :: MonadState World m => m Integer
process = drain $ \case
  ("ORE", need) -> ore += need
  (label, need) -> do
    Reaction yield ingredients <- reaction label
    borrowed <- borrow label
    let
      deficit = need - borrowed
      batches = multiplier deficit yield
      leftovers = batches * yield - deficit
    if deficit <= 0
      then share label $ negate deficit
      else do
        share label leftovers
        request $ fmap (batches *) <$> ingredients

drain :: MonadState World m => ((Label, Need) -> m ()) -> m Integer
drain f = loop
 where
  loop = pop >>= \case
    Nothing -> use ore
    Just req -> f req *> loop

multiplier :: Integer -> Integer -> Integer
multiplier need yield
  | m == 0 = d
  | otherwise = d + 1
  where (d, m) = need `divMod` yield

maximumFuel :: Graph -> Integer -> Integer
maximumFuel graph target = loop 0 target
 where
  loop lo hi = do
    let mid = between lo hi
    case compare (minimumOre graph mid) target of
      LT
        | minimumOre graph (mid + 1) >= target -> mid
        | otherwise -> loop mid hi
      EQ -> mid
      GT -> loop lo mid

between :: Integer -> Integer -> Integer
between lo hi = lo + (hi - lo) `div` 2

type Label = Text
type Need = Integer
type Yield = Integer

data Reaction = Reaction Yield (Seq (Label, Need))

type Graph = HashMap Label Reaction

data World = World
  { _reactions :: Graph
  , _queue :: Seq (Label, Need)
  , _extras :: HashMap Label Yield
  , _ore :: Integer
  }

reactions :: Lens' World Graph
reactions = lens _reactions $ \w r -> w { _reactions = r }

queue :: Lens' World (Seq (Label, Need))
queue = lens _queue $ \w q -> w { _queue = q }

extras :: Lens' World (HashMap Label Yield)
extras = lens _extras $ \w e -> w { _extras = e }

ore :: Lens' World Integer
ore = lens _ore $ \w o -> w { _ore = o }

reaction :: MonadState World m => Label -> m Reaction
reaction label = (! label) <$> use reactions

pop :: MonadState World m => m (Maybe (Label, Need))
pop = do
  q <- use queue
  case q of
    Empty -> pure Nothing
    x :<| xs -> do
      queue .= xs
      pure $ Just x

request :: MonadState World m => Seq (Label, Need) -> m ()
request ingredients = queue %= (<> ingredients)

share :: MonadState World m => Label -> Yield -> m ()
share label yield = extras %= (HashMap.insert label yield)

borrow :: MonadState World m => Label -> m Yield
borrow label = HashMap.lookupDefault 0 label <$> use extras

parseThrows :: Parser a -> Text -> IO a
parseThrows parser =
  either (throwIO . userError) pure . parseOnly (skipSpace *> parser)

parseGraph :: Parser Graph
parseGraph = HashMap.fromList <$> newlineSep parseReaction

parseReaction :: Parser (Label, Reaction)
parseReaction = do
  ingredients <- commaSep parseIngredient
  skipSpace
  void $ string "=>"
  skipSpace
  (result, n) <- parseIngredient
  pure (result, Reaction n $ Seq.fromList ingredients)

parseIngredient :: Parser (Label, Integer)
parseIngredient = do
  need <- decimal
  skipSpace
  (, need) <$> parseLabel

parseLabel :: Parser Text
parseLabel = pack <$> many1 (satisfy isUpper)

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy1` (char ',' <* skipSpace))

newlineSep :: Parser a -> Parser [a]
newlineSep = (`sepBy1` (char '\n' <* skipSpace))
