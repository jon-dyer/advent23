{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DaySeven where

import Relude.Unsafe ((!!))
import Text.Parsec (char, many1, spaces)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (nat)

data Card = A | K | Q | J | T | Nine | Eight | Seven | Six | Five | Four | Three | Two
  deriving stock (Eq, Show, Ord)

compareJokerwise :: Card -> Card -> Ordering
compareJokerwise J T = GT
compareJokerwise J Nine = GT
compareJokerwise J Eight = GT
compareJokerwise J Seven = GT
compareJokerwise J Six = GT
compareJokerwise J Five = GT
compareJokerwise J Four = GT
compareJokerwise J Three = GT
compareJokerwise J Two = GT
compareJokerwise T J = LT
compareJokerwise Nine J = LT
compareJokerwise Eight J = LT
compareJokerwise Seven J = LT
compareJokerwise Six J = LT
compareJokerwise Five J = LT
compareJokerwise Four J = LT
compareJokerwise Three J = LT
compareJokerwise Two J = LT
compareJokerwise J J = EQ
compareJokerwise a b = compare a b

data Kind = FiveOf | FourOf | FullHouse | ThreeOf | TwoPair | Pair | High
  deriving stock (Eq, Show, Ord)

data Hand where
  Hand :: [Card] -> Hand
  deriving stock (Eq, Show)

newtype Bid = Bid Int
  deriving stock (Eq, Show)

day7pt1 :: Text -> Text
day7pt1 input = case parseIt input of
  Left e -> show e
  Right hs -> show $ priceEm $ sortEm Jack hs

day7pt2 :: Text -> IO Text
day7pt2 input =
  let res :: IO Text
      res = evaluateWHNF $ case parseIt input of
        Left e ->
          show e
        Right hs ->
          show $ priceEm $ sortEm Joker hs
   in do
        res

priceEm :: [(Hand, Bid)] -> Int
priceEm hs = sum $ uncurry (*) <$> zip [1 ..] ((\(_, Bid b) -> b) <$> hs)

data Rule = Jack | Joker

categorize :: Rule -> Hand -> Kind
categorize Jack (Hand cs) =
  case sortWith Down (length <$> group (sort cs)) of
    [5] -> FiveOf
    4 : _ -> FourOf
    3 : 2 : _ -> FullHouse
    3 : _ -> ThreeOf
    2 : 2 : _ -> TwoPair
    2 : _ -> Pair
    _ -> High
categorize Joker (Hand cs) =
  let jokers = length $ filter (== J) cs
      rest = filter (/= J) cs
   in case jokers of
        0 -> categorize Jack (Hand cs)
        1 -> case categorize Jack (Hand rest) of
          High -> Pair
          Pair -> ThreeOf
          TwoPair -> FullHouse
          ThreeOf -> FourOf
          FourOf -> FiveOf
          x -> x
        2 -> case categorize Jack (Hand rest) of
          High -> ThreeOf
          Pair -> FourOf
          ThreeOf -> FiveOf
          x -> x
        3 -> case categorize Jack (Hand rest) of
          High -> FourOf
          Pair -> FiveOf
          x -> x
        4 -> FiveOf
        5 -> FiveOf

handNo :: Int -> Hand -> Card
handNo i (Hand cs) = cs !! i

sortEm :: Rule -> [(Hand, Bid)] -> [(Hand, Bid)]
sortEm r =
  let m = case r of
        Jack -> compare
        Joker -> compareJokerwise
   in sortBy $
        flip $
          comparing (categorize r . fst)
            <> (\a b -> m ((handNo 0 . fst) a) ((handNo 0 . fst) b))
            <> (\a b -> m ((handNo 1 . fst) a) ((handNo 1 . fst) b))
            <> (\a b -> m ((handNo 2 . fst) a) ((handNo 2 . fst) b))
            <> (\a b -> m ((handNo 3 . fst) a) ((handNo 3 . fst) b))
            <> (\a b -> m ((handNo 4 . fst) a) ((handNo 4 . fst) b))

parseIt :: Text -> Either Parsec.ParseError [(Hand, Bid)]
parseIt =
  Parsec.parse
    parseHandBids
    empty

parseHandBids :: Parsec.Parsec Text () [(Hand, Bid)]
parseHandBids =
  do
    many1
      ( do
          cs <- many1 parseCard
          spaces
          b <- nat
          spaces
          return (Hand cs, Bid b)
      )

parseCard :: Parsec.ParsecT Text () Identity Card
parseCard =
  (A <$ char 'A')
    <|> (K <$ char 'K')
    <|> (Q <$ char 'Q')
    <|> (J <$ char 'J')
    <|> (T <$ char 'T')
    <|> (Nine <$ char '9')
    <|> (Eight <$ char '8')
    <|> (Seven <$ char '7')
    <|> (Six <$ char '6')
    <|> (Five <$ char '5')
    <|> (Four <$ char '4')
    <|> (Three <$ char '3')
    <|> (Two <$ char '2')
