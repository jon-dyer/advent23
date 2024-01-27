module DayTen where

import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence as S (null, unfoldr)
import Lib (manySeq)
import Text.Parsec (char, endOfLine)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (int)

-- data Pipe =  (╔) | (╗) | (╚) | (╝) | (═) | (║)

data Connecting = UpDown | LeftRight | UpRight | UpLeft | DownRight | DownLeft | Ground | Starting

upDown :: Parsec.Parsec Text () Connecting
upDown = UpDown <$ char '|'

leftRight :: Parsec.Parsec Text () Connecting
leftRight = LeftRight <$ char '-'

upRight :: Parsec.Parsec Text () Connecting
upRight = DownRight <$ char 'F'

upLeft :: Parsec.Parsec Text () Connecting
upLeft = DownLeft <$ char '7'

downRight :: Parsec.Parsec Text () Connecting
downRight = UpRight <$ char 'L'

downLeft :: Parsec.Parsec Text () Connecting
downLeft = UpLeft <$ char 'J'

ground :: Parsec.Parsec Text () Connecting
ground = Ground <$ char '.'

starting :: Parsec.Parsec Text () Connecting
starting = Starting <$ char 'S'

day10pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day10pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show 0

day10pt2 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day10pt2 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show 0

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
