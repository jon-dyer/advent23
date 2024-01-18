{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayEight where

import Data.Map.Strict ((!))
import Data.Stream.Infinite (Stream)
import Data.Stream.Infinite qualified as Stream
import Text.Parsec (char, count, letter, many1, spaces, string)
import Text.Parsec qualified as Parsec

type Index = Map Text (Text, Text)

data Direction = L | R
  deriving stock (Eq, Show)

day8pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day8pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ solve hs

solve :: (NonEmpty Direction, Index) -> Int
solve (d, n) =
  let ds :: Stream Direction
      ds = Stream.cycle d
   in plumb 0 "AAA" ds n

plumb :: Int -> Text -> Stream Direction -> Index -> Int
plumb i "ZZZ" _ _ = i
plumb i k ds n =
  let val = n ! k
      next = case Stream.head ds of
        L -> fst val
        R -> snd val
      nd = Stream.drop 1 ds
   in plumb (i + 1) next nd n

parseIt :: Text -> Either Parsec.ParseError (NonEmpty Direction, Index)
parseIt =
  Parsec.parse
    parseDay8
    empty

parseDay8 :: Parsec.Parsec Text () (NonEmpty Direction, Index)
parseDay8 =
  do
    dirs <- many1 ((L <$ char 'L') <|> (R <$ char 'R'))
    spaces
    nodes <- many1 parseIndex
    return (fromList dirs, fromList nodes)

parseIndex :: Parsec.ParsecT Text () Identity (Text, (Text, Text))
parseIndex =
  do
    k <- toText <$> count 3 letter
    _ <- spaces >> char '=' >> spaces >> char '('
    l <- toText <$> count 3 letter
    _ <- string ", "
    r <- toText <$> count 3 letter
    _ <- char ')'
    spaces
    return (k, (toText l, toText r))

-- day8pt2 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
-- day8pt2 input =
-- evaluateWHNF $ case parseIt Joker input of
-- Left e ->
-- show e
-- Right hs ->
