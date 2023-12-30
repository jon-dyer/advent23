module DayTwo (Game (..), parseLine, Pull (..), Cubes (..), Possible (..), possible, Bag (..)) where

import Relude.Unsafe (read)
import Text.Parsec (char, digit, many1, spaces, string, try)
import Text.Parsec qualified as Parsec

data Cube
  = Red Int
  | Green Int
  | Blue Int
  deriving (Eq, Show)

cubeStr :: String -> Int -> Cube
cubeStr str =
  case str of
    "red" -> Red
    "green" -> Green
    "blue" -> Blue

data Cubes = Cubes
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Eq, Show)

newtype Bag = Bag Cubes
  deriving (Eq, Show)

newtype Pull = Pull Cubes
  deriving (Eq, Show)

newPull =
  Pull
    Cubes
      { red = 0,
        green = 0,
        blue = 0
      }

groupPulls :: [[Cube]] -> [Pull]
groupPulls = map (foldr addCubeCount newPull)

data Game where
  Game ::
    { iteration :: Int,
      pulls :: [Pull]
    } ->
    Game
  deriving (Eq, Show)

gameParser = string "Game"

addCubeCount :: Cube -> Pull -> Pull
addCubeCount (Red val) (Pull pull) =
  Pull (pull {red = red pull + val})
addCubeCount (Green val) (Pull pull) =
  Pull (pull {green = green pull + val})
addCubeCount (Blue val) (Pull pull) =
  Pull (pull {blue = blue pull + val})

cube :: forall {u}. Parsec.ParsecT Text u Identity Cube
cube = do
  val <- read <$> many1 digit
  spaces
  c <- cubeStr <$> (try (string "red") <|> try (string "blue") <|> string "green")
  return (c val)

pullParser =
  many1 (cube <* optional (string ", "))

wholeThing :: forall {u}. Parsec.ParsecT Text u Identity Game
wholeThing = do
  _ <- gameParser
  spaces
  iter <- read <$> many1 digit
  _ <- char ':' >> spaces
  ps <- many1 (pullParser <* optional (string "; "))
  return
    Game
      { iteration = iter,
        pulls = groupPulls ps
      }

parseLine :: Text -> Either Parsec.ParseError Game
parseLine =
  Parsec.parse
    wholeThing
    empty

data Possible
  = Impossible
  | Possible
  deriving (Eq, Show)

possible :: Bag -> Pull -> Possible
possible (Bag b) (Pull p) =
  if red b > red p
    || green b > green p
    || blue b > blue p
    then Impossible
    else Possible
