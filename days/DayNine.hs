{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayNine where

import Lib (nel)
import Text.Parsec (spaces)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (int)

data Hole
  = Hole !Int
  | Empty

day9pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day9pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ solve hs

solve :: NonEmpty Int -> Int
solve _ =
  0

nextLine :: forall {a}. (Num a) => [a] -> [a]
nextLine [] = []
nextLine [_] = []
nextLine (a : b : l) = b - a : nextLine l

parseIt :: Text -> Either Parsec.ParseError (NonEmpty Int)
parseIt =
  Parsec.parse
    parseHistory
    empty

parseHistory :: Parsec.Parsec Text () (NonEmpty Int)
parseHistory =
  nel $ int <* spaces
