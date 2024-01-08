{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module DayFive where

import Data.Foldable (Foldable (minimum))
import Text.Parsec (many1, spaces, string)
import Text.Parsec qualified as Parsec
import Text.Parsec.Number (nat)

newtype Seed = Seed [Int]
  deriving stock (Eq, Show)

data Range = Range
  { destinationStart :: Int,
    sourceStart :: Int,
    rangeLength :: Int
  }
  deriving stock (Eq, Show)

data Mapper
  = SeedToSoil Range
  | SoilToFertilizer Range
  | FertilizerToWater Range
  | WaterToLight Range
  | LightToTemp Range
  | TempToHumidity Range
  | HumidityToLocation Range
  deriving stock (Eq, Show)

toRange :: Mapper -> Range
toRange (SeedToSoil m) = m
toRange (SoilToFertilizer m) = m
toRange (FertilizerToWater m) = m
toRange (WaterToLight m) = m
toRange (LightToTemp m) = m
toRange (TempToHumidity m) = m
toRange (HumidityToLocation m) = m

lineParser ::
  forall {u}.
  Parsec.ParsecT
    Text
    u
    Identity
    (Seed, [[Mapper]])
lineParser =
  do
    _ <- string "seeds:"
    spaces
    iter <- many1 $ nat <* optional spaces
    stss <- parseMapRanges "seed-to-soil map:" SeedToSoil
    stfs <- parseMapRanges "soil-to-fertilizer map:" SoilToFertilizer
    ftws <- parseMapRanges "fertilizer-to-water map:" FertilizerToWater
    wtls <- parseMapRanges "water-to-light map:" WaterToLight
    ltts <- parseMapRanges "light-to-temperature map:" LightToTemp
    tths <- parseMapRanges "temperature-to-humidity map:" TempToHumidity
    htls <- parseMapRanges "humidity-to-location map:" HumidityToLocation
    return (Seed iter, [stss, stfs, ftws, wtls, ltts, tths, htls])

parseMapRanges :: String -> (Range -> a) -> Parsec.ParsecT Text u Identity [a]
parseMapRanges header constructor =
  do
    _ <- string header
    _ <- spaces
    many1 $ do
      st <- nat <* spaces
      sp <- nat <* spaces
      r <- nat <* spaces
      return $ constructor (Range st sp r)

parseIt :: Text -> Either Parsec.ParseError (Seed, [[Mapper]])
parseIt =
  Parsec.parse
    lineParser
    empty

srcToDst :: Int -> Range -> Int
srcToDst i (Range fd fs _) =
  i + (fd - fs)

theOneTrueRange :: Int -> [Range] -> Range
theOneTrueRange i rs =
  fromMaybe (Range i i 0) (find (\(Range _ src l) -> i >= src && i < src + l) rs)

seedLocation :: [[Mapper]] -> Int -> Int
seedLocation m i =
  foldl'
    findDestination
    i
    ((toRange <$>) <$> m)

findDestination :: Int -> [Range] -> Int
findDestination i a =
  srcToDst i $ theOneTrueRange i a

day5pt1 :: Text -> Int
day5pt1 text =
  let Right (Seed seeds, maps) = parseIt text
   in minimum $ map (seedLocation maps) seeds
