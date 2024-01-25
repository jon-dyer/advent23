{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayNine where

import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence as S (null, unfoldr)
import Lib (manySeq)
import Text.Parsec (char, endOfLine)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (int)

type Extrapolater = (Seq (Seq Int) -> Int)

day9pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day9pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ solve extrapolateFuture hs

day9pt2 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day9pt2 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right hs -> show $ solve extrapolateHistory hs

solve :: Extrapolater -> Seq (Seq Int) -> Int
solve e ne =
  sum $ solveLine e <$> ne

solveLine :: Extrapolater -> Seq Int -> Int
solveLine e is =
  let initialtree :: Seq (Seq Int)
      initialtree =
        S.unfoldr
          ( \prev ->
              if S.null prev
                then Nothing
                else
                  if all (== 0) prev
                    then Just (prev, Empty)
                    else Just (prev, nextLine prev)
          )
          is
      zeroed = (\a -> if all (== 0) a then 0 <| a else a) <$> initialtree
   in e zeroed

extrapolateFuture :: Seq (Seq Int) -> Int
extrapolateFuture Empty = 0
extrapolateFuture (rs :|> (bTail :|> b) :|> (_ :|> a)) =
  let newB = a + b
   in extrapolateFuture (rs |> (bTail |> b |> newB))
extrapolateFuture (_ :|> _ :|> _) = 0
extrapolateFuture (Empty :|> (_ :|> a)) = a
extrapolateFuture (Empty :|> _) = 0

extrapolateHistory :: Seq (Seq Int) -> Int
extrapolateHistory Empty = 0
extrapolateHistory (rs :|> (bottom :<| bottomTail) :|> (top :<| _)) =
  let newB = bottom - top
   in extrapolateHistory (rs |> (newB <| bottom <| bottomTail))
extrapolateHistory (_ :|> _ :|> _) = 0
extrapolateHistory (Empty :|> (a :<| _)) = a
extrapolateHistory (Empty :|> _) = 0

nextLine :: forall {a}. (Num a) => Seq a -> Seq a
nextLine Empty = Empty
nextLine (_ :<| Empty) = Empty
nextLine (a :<| b :<| l) = b - a <| nextLine (b <| l)

parseIt :: Text -> Either Parsec.ParseError (Seq (Seq Int))
parseIt =
  Parsec.parse
    parseHistory
    Prelude.empty

parseHistory :: Parsec.Parsec Text () (Seq (Seq Int))
parseHistory =
  manySeq $
    do
      h <- int
      t <- manySeq $ char ' ' *> int
      _ <- optional endOfLine
      return $ h <| t
