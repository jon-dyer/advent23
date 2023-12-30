module DayTwo (Game (..), parseLine, pline, Pull (..)) where

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

data Pull = Pull
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Eq, Show)

newPull =
  Pull
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

newGame :: Int -> Game
newGame i =
  Game
    { iteration = i,
      pulls = []
    }

gameParser = string "Game"

addCubeCount :: Cube -> Pull -> Pull
addCubeCount (Red val) pull =
  pull {red = red pull + val}
addCubeCount (Green val) pull =
  pull {green = green pull + val}
addCubeCount (Blue val) pull =
  pull {blue = blue pull + val}

cube :: forall {u}. Parsec.ParsecT Text u Identity Cube
cube = do
  val <- read <$> many1 digit
  spaces
  c <- cubeStr <$> (try (string "red") <|> try (string "blue") <|> string "green")
  return (c val)

pull =
  many1 (cube <* optional (string ", "))

wholeThing :: forall {u}. Parsec.ParsecT Text u Identity Game
wholeThing = do
  _ <- gameParser
  spaces
  iter <- read <$> many1 digit
  _ <- char ':' >> spaces
  ps <- many1 (pull <* optional (string "; "))
  return
    ( Game
        { iteration = iter,
          pulls = groupPulls ps
        }
    )

pline = Parsec.parse wholeThing empty

parseLine :: Text -> Either Parsec.ParseError Game
parseLine =
  Parsec.parse
    wholeThing
    empty
