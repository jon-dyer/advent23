{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DaySeven where

import Data.List (partition)
import Relude.Unsafe ((!!))
import Text.Parsec (char, many1, spaces)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (nat)

data Card = A | K | Q | Ja | T | Nine | Eight | Seven | Six | Five | Four | Three | Two | Jo
  deriving stock (Eq, Show, Ord)

data Kind = FiveOf | FourOf | FullHouse | ThreeOf | TwoPair | Pair | High
  deriving stock (Eq, Show, Ord)

data Hand where
  Hand :: [Card] -> Hand
  deriving stock (Eq, Show)

newtype Bid = Bid Int
  deriving stock (Eq, Show)

day7pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day7pt1 input =
  evaluateWHNF $
    case parseIt Jack input of
      Left e -> show e
      Right hs -> show $ priceEm $ sortEm hs

day7pt2 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day7pt2 input =
  evaluateWHNF $ case parseIt Joker input of
    Left e ->
      show e
    Right hs ->
      show $ priceEm $ sortEm hs

priceEm :: [(Hand, Bid)] -> Int
priceEm hs = sum $ uncurry (*) <$> zip [1 ..] ((\(_, Bid b) -> b) <$> hs)

data Rule = Jack | Joker

categorize :: Hand -> Kind
categorize (Hand cs) =
  let (jokers, rest) = first length $ partition (== Jo) cs
   in case jokers of
        0 -> case sortWith Down (length <$> group (sort rest)) of
          [5] -> FiveOf
          4 : _ -> FourOf
          3 : 2 : _ -> FullHouse
          3 : _ -> ThreeOf
          2 : 2 : _ -> TwoPair
          2 : _ -> Pair
          _allOneOfs -> High
        1 -> case categorize (Hand rest) of
          High -> Pair
          Pair -> ThreeOf
          TwoPair -> FullHouse
          ThreeOf -> FourOf
          FourOf -> FiveOf
          x -> x
        2 -> case categorize (Hand rest) of
          High -> ThreeOf
          Pair -> FourOf
          ThreeOf -> FiveOf
          x -> x
        3 -> case categorize (Hand rest) of
          High -> FourOf
          Pair -> FiveOf
          x -> x
        _restMustBeJokers -> FiveOf

handNo :: Int -> Hand -> Card
handNo i (Hand cs) = cs !! i

sortEm :: [(Hand, Bid)] -> [(Hand, Bid)]
sortEm =
  let getCard :: Int -> (Hand, Bid) -> Card
      getCard i (h, _) = handNo i h
   in sortBy $
        flip $
          comparing (categorize . fst)
            <> foldMap (comparing . getCard) [0 .. 4]

parseIt :: Rule -> Text -> Either Parsec.ParseError [(Hand, Bid)]
parseIt r =
  Parsec.parse
    (parseHandBids r)
    empty

parseHandBids :: Rule -> Parsec.Parsec Text () [(Hand, Bid)]
parseHandBids r =
  many1 $
    do
      cs <- many1 $ parseCard r
      spaces
      b <- nat
      spaces
      return (Hand cs, Bid b)

parseCard :: Rule -> Parsec.ParsecT Text () Identity Card
parseCard r =
  let j = case r of
        Jack -> Ja
        Joker -> Jo
   in (A <$ char 'A')
        <|> (K <$ char 'K')
        <|> (Q <$ char 'Q')
        <|> (j <$ char 'J')
        <|> (T <$ char 'T')
        <|> (Nine <$ char '9')
        <|> (Eight <$ char '8')
        <|> (Seven <$ char '7')
        <|> (Six <$ char '6')
        <|> (Five <$ char '5')
        <|> (Four <$ char '4')
        <|> (Three <$ char '3')
        <|> (Two <$ char '2')
