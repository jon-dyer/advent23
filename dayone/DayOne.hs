module DayOne (readCalibration, readCalibrations, sumCalibrations, parseFirstNumber, parseLastNumber, readTextyCali, readTextyCalis, sumTextyCalis) where

import Data.Char
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Text.Parsec (anyChar, try)
import Text.Parsec qualified as Parsec

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

numbersAsText :: [String]
numbersAsText = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

numericWord :: forall {u}. Parsec.ParsecT Text u Identity String
numericWord = asum (Parsec.try . Parsec.string <$> numbersAsText)

backwardsNumbers :: forall {u}. Parsec.ParsecT Text u Identity String
backwardsNumbers = asum (Parsec.try . Parsec.string . reverse <$> numbersAsText)

skipUntil p = try p <|> (anyChar >> skipUntil p)

number :: forall {u}. Parsec.ParsecT Text u Identity Int
number =
  (digitToInt <$> Parsec.try Parsec.digit) <|> (fromText <$> numericWord)

number' :: forall {u}. Parsec.ParsecT Text u Identity Int
number' =
  (digitToInt <$> Parsec.try Parsec.digit) <|> (fromText . reverse <$> backwardsNumbers)

parseFirstNumber :: Text -> Either Parsec.ParseError Int
parseFirstNumber = Parsec.parse (skipUntil number) empty

parseLastNumber :: Text -> Either Parsec.ParseError Int
parseLastNumber = Parsec.parse (skipUntil number') empty . Text.reverse

readTextyCali :: Text -> Either Parsec.ParseError Int
readTextyCali t =
  let firstDigit :: Either Parsec.ParseError Int
      firstDigit = parseFirstNumber t
      lastDigit :: Either Parsec.ParseError Int
      lastDigit = parseLastNumber t
      tens :: Either Parsec.ParseError Int
      tens = (10 *) <$> firstDigit
   in (+) <$> tens <*> lastDigit

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

readTextyCalis :: Text -> [Int]
readTextyCalis t =
  rights (readTextyCali <$> lines t)

sumCalibrations :: Text -> Int
sumCalibrations = sum . readCalibrations

sumTextyCalis :: Text -> Int
sumTextyCalis = sum . readTextyCalis
