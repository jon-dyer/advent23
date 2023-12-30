module DayTwo (Game (..), parseLine, pline) where

import Data.Char
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Relude.Unsafe (read)
import Text.Parsec (digit, many1, spaces, string)
import Text.Parsec qualified as Parsec

data Cube
  = Red Int
  | Green Int
  | Blue Int
  deriving (Eq, Show)

data Game where
  Game ::
    { iteration :: Int,
      red :: Int,
      green :: Int,
      blue :: Int
    } ->
    Game
  deriving (Eq, Show)

game = string "Game"

cube = do
  val <- many1 digit 

wholeThing :: forall {u}. Parsec.ParsecT Text u Identity Game
wholeThing = do
  game
  spaces
  iter <- read <$> many1 digit
  return
    Game
      { iteration = iter,
        red = 0,
        green = 0,
        blue = 0
      }

pline = Parsec.parse wholeThing empty

parseLine :: Text -> Either Parsec.ParseError Game
parseLine =
  Parsec.parse
    wholeThing
    empty
