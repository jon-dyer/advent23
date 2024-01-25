{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayEight where

import Data.HashMap.Strict (keys, (!))
import Data.Stream.Infinite (Stream)
import Data.Stream.Infinite qualified as Stream
import Data.Text qualified as T
import Lib (nel)
import Text.Parsec (char, count, letter, many1, spaces, string)
import Text.Parsec qualified as Parsec

type Index = HashMap Text (Text, Text)

data Direction = L | R
  deriving stock (Eq, Show)

type EndTest = Text -> Bool

day8pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day8pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ uncurry solve hs

solve :: NonEmpty Direction -> Index -> Int
solve d i =
  let ds :: Stream Direction
      ds = Stream.cycle d
   in plumb (== "ZZZ") ds i 0 "AAA"

ghostSolve :: (NonEmpty Direction, Index) -> Int
ghostSolve (d, i) =
  let starterKeys :: [Text]
      starterKeys = filter ((== 'A') . T.last) $ keys i
      ds :: Stream Direction
      ds = Stream.cycle d
      ended = (==) 'Z' . T.last
   in foldr (lcm . plumb ended ds i 0) 1 starterKeys

plumb :: EndTest -> Stream Direction -> Index -> Int -> Text -> Int
plumb et ds index i t =
  if et t
    then i
    else
      let val = index ! t
          next = case Stream.head ds of
            L -> fst val
            R -> snd val
          nd = Stream.drop 1 ds
       in plumb et nd index (i + 1) next

parseIt :: Text -> Either Parsec.ParseError (NonEmpty Direction, Index)
parseIt =
  Parsec.parse
    parseDay8
    empty

parseDay8 :: Parsec.Parsec Text () (NonEmpty Direction, Index)
parseDay8 =
  let lr = ((L <$ char 'L') <|> (R <$ char 'R'))
   in do
        dirs <- nel lr
        spaces
        nodes <- many1 parseIndex
        return (dirs, fromList nodes)

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

day8pt2 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day8pt2 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ ghostSolve hs
