{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayFour where

import Relude.Unsafe (read)
import Text.Parsec (char, digit, many1, spaces, string)
import Text.Parsec qualified as Parsec

data Game = Game [Int] [Int]
  deriving stock (Show, Eq)

day4pt1 :: Text -> Int
day4pt1 input =
  let games :: [Game]
      games = parseGames input
      points :: [Int]
      points = score <$> games
   in sum points

parseGames :: Text -> [Game]
parseGames t =
  rights $ parseGame <$> lines t

numbersParser :: Parsec.ParsecT Text u Identity [Int]
numbersParser =
  many1 (read <$> many1 digit <* spaces)

gameParser :: forall {u}. Parsec.ParsecT Text u Identity Game
gameParser = do
  _ <- string "Card" >> spaces
  _ <- many1 digit
  _ <- char ':' >> spaces
  ws <- numbersParser
  _ <- char '|' >> spaces
  hs <- numbersParser
  return $
    Game ws hs

parseGame :: Text -> Either Parsec.ParseError Game
parseGame =
  Parsec.parse
    gameParser
    empty

score :: Game -> Int
score (Game ws hs) =
  let matches = filter (`elem` ws) hs
   in case length matches of
        0 -> 0
        n -> 2 ^ (n - 1)
