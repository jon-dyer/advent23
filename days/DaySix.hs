{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DaySix where

import Relude.Unsafe (read)
import Text.Parsec (many1, spaces, string, (<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Char (digit)
import Text.Parsec.Number (nat)

data Race where
  Race :: Int -> Int -> Race
  deriving stock (Show, Eq)

parseIt :: Text -> Either Parsec.ParseError [Race]
parseIt =
  Parsec.parse
    racesParser
    empty

parseSpaced :: Text -> Either Parsec.ParseError Race
parseSpaced =
  Parsec.parse
    racesSpacelessParser
    empty

racesSpacelessParser :: Parsec.Parsec Text () Race
racesSpacelessParser =
  do
    _ <- string "Time:" >> spaces
    t <- spacyNumber <?> "time numbers wrong"
    _ <- string "Distance:" >> spaces
    d <- spacyNumber <?> "distance numbers wrong"
    return $ Race t d

numbers :: forall {u}. Parsec.ParsecT Text u Identity [Int]
numbers =
  many1 (nat <* spaces)

spacyNumber :: forall {u}. Parsec.ParsecT Text u Identity Int
spacyNumber =
  read <$> many1 (digit <* spaces)

racesParser :: Parsec.Parsec Text () [Race]
racesParser =
  do
    _ <- string "Time:" >> spaces
    ts <- numbers <?> "time numbers wrong"
    _ <- string "Distance:" >> spaces
    ds <- numbers <?> "distance numbers wrong"
    return $ uncurry Race <$> zip ts ds

day6pt1 :: Text -> Text
day6pt1 input =
  case solveAll <$> parseIt input of
    Left t -> show t
    Right a -> show a

day6pt2 :: Text -> Text
day6pt2 input =
  case length . winningRaces <$> parseSpaced input of
    Left t -> show t
    Right a -> show a

solveAll :: [Race] -> Int
solveAll rs =
  product $ length . winningRaces <$> rs

winningRaces :: Race -> [Int]
winningRaces (Race t d) =
  filter (> d) $ (\a -> (t - a) * a) <$> [1 .. (t - 1)]
