{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayFive where

import Control.Parallel.Strategies (parList, parMap, rdeepseq, rpar, using)
import Data.Foldable (Foldable (minimum))
import Text.Parsec (anyChar, many1, manyTill, spaces, string, try)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (nat)

newtype Seed = Seed [Int]
  deriving stock (Eq, Show)

data Range where
  Range ::
    { destinationStart :: Int,
      destinationStop :: Int,
      sourceStart :: Int,
      sourceStop :: Int
    } ->
    Range
  deriving stock (Eq, Show)

data SeedRange where
  SeedRange :: {start :: Int, stop :: Int} -> SeedRange
  deriving stock (Eq, Show)

point :: Int -> Range
point i = Range i i i i

inputParser :: Parsec.ParsecT Text u Identity a -> Parsec.ParsecT Text u Identity (a, [[Range]])
inputParser sp =
  do
    seed <- sp
    ranges <- many1 parseMapRanges
    return (seed, ranges)

seedParser :: forall {u}. Parsec.ParsecT Text u Identity Seed
seedParser =
  do
    _ <- string "seeds:"
    spaces
    ss <- many1 $ nat <* optional spaces
    return $ Seed ss

seedRangeParser :: forall {u}. Parsec.ParsecT Text u Identity [SeedRange]
seedRangeParser =
  do
    _ <- string "seeds:"
    spaces
    many1 $ do
      strt <- nat
      spaces
      stp <- nat <* optional spaces
      return $ SeedRange strt (strt + stp - 1)

parseMapRanges :: Parsec.ParsecT Text u Identity [Range]
parseMapRanges =
  do
    _ <- manyTill anyChar $ try (string "map:")
    _ <- spaces
    many1 $ do
      dst <- nat <* spaces
      sr <- nat <* spaces
      r <- nat <* spaces
      return $ Range dst (dst + r - 1) sr (sr + r - 1)

parseIt :: Text -> Either Parsec.ParseError (Seed, [[Range]])
parseIt =
  Parsec.parse
    (inputParser seedParser)
    empty

parseItSeedRanges :: Text -> Either Parsec.ParseError ([SeedRange], [[Range]])
parseItSeedRanges =
  Parsec.parse
    (inputParser seedRangeParser)
    empty

srcToDst :: Int -> Range -> Int
srcToDst i (Range fd _ fs _) =
  i + (fd - fs)

dstToSrc :: Int -> Range -> Int
dstToSrc i (Range fd _ fs _) =
  i + (fs - fd)

inStartRange :: Int -> Range -> Bool
inStartRange i (Range _ _ ss se) = i >= ss && i <= se

inDestRange :: Int -> Range -> Bool
inDestRange i (Range ds de _ _) = i >= ds && i <= de

inSeedRange :: Int -> SeedRange -> Bool
inSeedRange i (SeedRange ss se) = i >= ss && i <= se

mapSrcToDst :: Int -> [Range] -> Range
mapSrcToDst i rs =
  fromMaybe (point i) (find (inStartRange i) rs)

mapDstToSrc :: Int -> [Range] -> Range
mapDstToSrc i rs =
  let found = listToMaybe (filter (inDestRange i) rs `using` parList rpar)
   in fromMaybe (point i) found

seedLocation :: [[Range]] -> Int -> Int
seedLocation =
  let findDestination :: Int -> [Range] -> Int
      findDestination i' = (srcToDst i' . mapSrcToDst i')
   in flip $ foldl' findDestination

findSource :: [[Range]] -> Int -> Int
findSource rs i =
  let findDestination :: Int -> [Range] -> Int
      findDestination i' =
        (dstToSrc i' . mapDstToSrc i')
   in foldl' findDestination i rs

day5pt1 :: forall {m :: Type -> Type}. (MonadIO m) => Text -> m Text
day5pt1 input =
  evaluateWHNF $ case parseIt input of
    Left e -> show e
    Right (Seed seeds, maps) -> show $ minimum $ seedLocation maps <$> seeds

basicForLoop :: [[Range]] -> Int -> Int -> Int -> Int
basicForLoop ms prev current stp
  | current > stp = min prev (seedLocation ms current)
  | otherwise = basicForLoop ms (min (seedLocation ms current) prev) (current + 1) stp

pt2 :: [SeedRange] -> [[Range]] -> Int -> Int
pt2 seedRanges maps i =
  let isSeed :: Bool
      isSeed = or $ parMap rdeepseq (inSeedRange src) seedRanges
      src = findSource maps i
   in if isSeed
        then i
        else pt2 seedRanges maps (i + 1)

day5pt2 :: Text -> Int
day5pt2 text =
  let Right (seedRanges, maps) = parseItSeedRanges text
   in pt2 seedRanges (reverse maps) 0
