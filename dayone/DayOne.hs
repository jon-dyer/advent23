module DayOne (readCalibration, readCalibrations, sumCalibrations, parseNumber) where

import Data.Char
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Text.Parsec qualified as Parsec

zero = Parsec.string "zero"

oneString = Parsec.string "one"

two = Parsec.string "two"

three = Parsec.string "three"

four = Parsec.string "four"

five = Parsec.string "five"

six = Parsec.string "six"

seven = Parsec.string "seven"

eight = Parsec.string "eight"

nine = Parsec.string "nine"

-- myNumWordParsers = [zero, oneString, two, three, four, five, six, seven, eight, nine]

numericWord = zero <|> oneString <|> two <|> three <|> four <|> five <|> six <|> seven <|> eight <|> nine
--
number = (Parsec.many1 Parsec.digit) <|> numericWord

-- number = Parsec.many Parsec.digit -- (Parsec.try (Padigit)) <|> (Parsec.try numericWord)

parseNumber :: Text -> Either Parsec.ParseError String
parseNumber = Parsec.parse number ""

readTextyCali :: Text -> Either String Int
readTextyCali t =
  let findDigit = Text.take 1 . Text.dropWhile (not . isDigit)
      toInt :: Text -> Either String Int
      toInt = fmap fst . decimal
      firstDigit = findDigit t
      lastDigit = findDigit (Text.reverse t)
   in toInt (firstDigit <> lastDigit)

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
