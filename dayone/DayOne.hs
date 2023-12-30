module DayOne (readCalibration, readCalibrations, sumCalibrations, parseFirstNumber, parseLastNumber) where

import Data.Char
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Text.Parsec (anyChar, try)
import Text.Parsec qualified as Char
import Text.Parsec qualified as Parsec

  {-
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
-}

fromText :: String -> Int
fromText n = case n of
  "zero" -> 0
  "one" -> 1
  "two" -> 2
  "three" -> 3
  "four" -> 4
  "five" -> 5
  "six" -> 6
  "seven" -> 7
  "eight" -> 8
  "nine" -> 9

numericWord :: forall {u}. Parsec.ParsecT Text u Identity String
numericWord = foldr ((<|>)) empty (fmap Parsec.string ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])

-- numericWord = zero <|> oneString <|> two <|> three <|> four <|> five <|> six <|> seven <|> eight <|> nine

skipUntil p = try p <|> (anyChar >> skipUntil p)

skipUntil' p skipper = try p <|> (skipper >> skipUntil' p skipper)

--
number :: forall {u}. Parsec.ParsecT Text u Identity Int
number =
  (digitToInt <$> Parsec.try Parsec.digit) <|> (fromText <$> numericWord)

-- numbers =
-- Parsec.skipMany (Parsec.n number) >> number

-- number = Parsec.many Parsec.digit -- (Parsec.try (Padigit)) <|> (Parsec.try numericWord)

-- parseFirstNumber :: Text -> Either Parsec.ParseError Int
parseFirstNumber :: Text -> Either Parsec.ParseError Int
parseFirstNumber = Parsec.parse (skipUntil number) empty

parseLastNumber :: Text -> Either Parsec.ParseError Int
parseLastNumber = Parsec.parse (Parsec.skipMany Char.anyChar >> number) empty

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
