{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayNine where

import Lib (nel)
import Text.Parsec (spaces)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (int)

day9pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day9pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ solve hs

solve :: NonEmpty Int -> Int
solve _ =
  0

parseIt :: Text -> Either Parsec.ParseError (NonEmpty Int)
parseIt =
  Parsec.parse
    parseHistory
    empty

parseHistory :: Parsec.Parsec Text () (NonEmpty Int)
parseHistory =
  nel $ int <* spaces
