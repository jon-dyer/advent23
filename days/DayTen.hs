{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module DayTen where

import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence as S (null, unfoldr)
import Lib (manySeq)
import Text.Parsec (char, endOfLine)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (int)

type Extrapolater = (Seq (Seq Int) -> Int)

type Pipe =  ╔ | ╗ | ╚ | ╝ | ═ | ║

day10pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day10pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ solve extrapolateFuture hs

day10pt2 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day10pt2 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ solve extrapolateHistory hs

parseIt :: Text -> Either Parsec.ParseError (Seq (Seq Int))
parseIt =
  Parsec.parse
    parseMaze
    Prelude.empty

parseMaze :: Parsec.Parsec Text () (Seq (Seq Int))
parseMaze =
  manySeq $
    do
      h <- int
      t <- manySeq $ char ' ' *> int
      _ <- optional endOfLine
      return $ h <| t
