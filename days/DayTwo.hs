module DayTwo (Game (..), parseLine, Pull (..), Cubes (..), GamePossible (..), pullPossible, gamePossible, Bag (..), sumPossibleGames, standardBag, smallestBag, power, sumPowers) where

import Relude.Unsafe (read)
import Text.Parsec (char, digit, many1, spaces, string, try)
import Text.Parsec qualified as Parsec

data Cube
  = Red Int
  | Green Int
  | Blue Int
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

newtype Bag = Bag Cubes
  deriving stock (Eq, Show)

newtype Pull = Pull Cubes
  deriving stock (Eq, Show)

newPull :: Pull
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
    { ident :: Int,
      pulls :: [Pull]
    } ->
    Game
  deriving stock (Eq, Show)

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

pullParser :: Parsec.ParsecT Text u Identity [Cube]
pullParser =
  many1 (cube <* optional (string ", "))

gameParser :: forall {u}. Parsec.ParsecT Text u Identity Game
gameParser = do
  _ <- string "Game"
  spaces
  iter <- read <$> many1 digit
  _ <- char ':' >> spaces
  ps <- many1 (pullParser <* optional (string "; "))
  return
    Game
      { ident = iter,
        pulls = groupPulls ps
      }

parseLine :: Text -> Either Parsec.ParseError Game
parseLine =
  Parsec.parse
    gameParser
    empty

pullPossible :: Bag -> Pull -> GamePossible
pullPossible (Bag b) (Pull p) =
  if red b < red p
    || green b < green p
    || blue b < blue p
    then Impossible
    else Possible

data GamePossible
  = Impossible
  | Possible
  deriving stock (Eq, Show)

gamePossible :: Bag -> Game -> GamePossible
gamePossible b (Game _ (ps :: [Pull])) =
  let gps :: [GamePossible]
      gps = pullPossible b <$> ps
   in foldr
        ( \val soFar ->
            case (val, soFar) of
              (_, Impossible) -> Impossible
              (Impossible, _) -> Impossible
              (Possible, Possible) -> Possible
        )
        Possible
        gps

sumPossibleGames :: Bag -> Text -> Int
sumPossibleGames b t =
  let thing = rights (parseLine <$> lines t)
      possibles = filter (\g -> Possible == gamePossible b g) thing
   in sum (ident <$> possibles)

standardBag :: Bag
standardBag = Bag Cubes {red = 12, green = 13, blue = 14}

maxPull :: Pull -> Pull -> Pull
maxPull (Pull p1) (Pull p2) =
  Pull
    ( Cubes
        (max (red p1) (red p2))
        (max (green p1) (green p2))
        (max (blue p1) (blue p2))
    )

smallestBag :: Game -> Bag
smallestBag (Game _ (ps :: [Pull])) =
  let Pull mp = foldr maxPull (Pull (Cubes 0 0 0)) ps
   in Bag mp

power :: Bag -> Int
power (Bag g) =
  red g * green g * blue g

sumPowers :: Text -> Int
sumPowers t =
  let games = rights (parseLine <$> lines t)
      sb = smallestBag <$> games
      ps = power <$> sb
   in sum ps
