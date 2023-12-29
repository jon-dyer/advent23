module DayOne (readCalibration, readCalibrations, sumCalibrations) where

import Data.Char
import Data.Text qualified as Text
import Data.Text.Read (decimal)

readCalibration :: Text -> Either String Int
readCalibration t =
  let findDigit = Text.take 1 . Text.dropWhile (not . isDigit)
      toInt :: Text -> Either String Int
      toInt = fmap fst . decimal
      firstDigit = findDigit t
      lastDigit = findDigit (Text.reverse t)
   in toInt (firstDigit <> lastDigit)

readCalibrations :: Text -> [Int]
readCalibrations t =
  rights (readCalibration <$> lines t)

sumCalibrations :: Text -> Int
sumCalibrations = sum . readCalibrations
