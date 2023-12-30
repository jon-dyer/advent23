module DayTwo (Game (..), parseLine, pline) where

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

data Game where
  Game ::
    { iteration :: Int,
      red :: Int,
      green :: Int,
      blue :: Int
    } ->
    Game
  deriving (Eq, Show)

gameParser = string "Game"

addCubeCount :: Cube -> Game -> Game
addCubeCount (Red val) game =
  game {red = red game + val}
addCubeCount (Green val) game =
  game {green = green game + val}
addCubeCount (Blue val) game =
  game {blue = blue game + val}

cube :: forall {u}. Parsec.ParsecT Text u Identity Cube
cube = do
  val <- read <$> many1 digit
  spaces
  c <- cubeStr <$> (try (string "red") <|> try (string "blue") <|> string "green")
  return (c val)

wholeThing :: forall {u}. Parsec.ParsecT Text u Identity Game
wholeThing = do
  _ <- gameParser
  spaces
  iter <- read <$> many1 digit
  _ <- char ':' >> spaces
  c <- cube
  return
    ( addCubeCount
        c
        ( Game
            { iteration = iter,
              red = 0,
              green = 0,
              blue = 0
            }
        )
    )

pline = Parsec.parse wholeThing empty

parseLine :: Text -> Either Parsec.ParseError Game
parseLine =
  Parsec.parse
    wholeThing
    empty
