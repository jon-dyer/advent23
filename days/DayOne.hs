module DayOne (readCalibration, readCalibrations, sumCalibrations, parseFirstNumber, parseLastNumber, readTextyCali, readTextyCalis, sumTextyCalis) where

import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Lib (wholeNumbers)
import Relude.Extra (foldl1')
import Text.Parsec (anyChar, string, try)
import Text.Parsec qualified as Parsec

numericWord :: forall {u}. NonEmpty Text -> Parsec.ParsecT Text u Identity Int
numericWord ns =
  let intAndText :: NonEmpty (Parsec.ParsecT Text u Identity String, Int)
      intAndText = NE.zip (try . string . toString <$> ns) wholeNumbers
   in foldl1' (<|>) (uncurry ($>) <$> intAndText)

numbersAsText :: NonEmpty Text
numbersAsText =
  "zero" :| ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

skipUntil :: forall {u} {a}. Parsec.ParsecT Text u Identity a -> Parsec.ParsecT Text u Identity a
skipUntil p = try p <|> (anyChar >> skipUntil p)

parseNumericWord :: forall {u}. NonEmpty Text -> Parsec.ParsecT Text u Identity Int
parseNumericWord ns =
  (digitToInt <$> Parsec.try Parsec.digit) <|> numericWord ns

number :: forall {u}. Parsec.ParsecT Text u Identity Int
number = parseNumericWord numbersAsText

rebmun :: forall {u}. Parsec.ParsecT Text u Identity Int
rebmun = parseNumericWord $ T.reverse <$> numbersAsText

parseFirstNumber :: Text -> Either Parsec.ParseError Int
parseFirstNumber = Parsec.parse (skipUntil number) empty

parseLastNumber :: Text -> Either Parsec.ParseError Int
parseLastNumber = Parsec.parse (skipUntil rebmun) empty . T.reverse

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
  let findDigit = T.take 1 . T.dropWhile (not . isDigit)
      toInt :: Text -> Either String Int
      toInt = fmap fst . decimal
      firstDigit = findDigit t
      lastDigit = findDigit (T.reverse t)
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
