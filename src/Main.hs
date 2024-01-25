module Main where

import Data.Text (append)
import DayEight (day8pt1, day8pt2)
import DayFive (day5pt1, day5pt2)
import DayFour (day4pt1, day4pt2)
import DayOne (sumCalibrations, sumTextyCalis)
import DaySeven (day7pt1, day7pt2)
import DaySix (day6pt1, day6pt2)
import DayThree (day3pt1, day3pt2)
import DayTwo (standardBag, sumPossibleGames, sumPowers)

main :: IO ()
main = do
  dayOneContent <- decodeUtf8 <$> readFileBS "inputs/day1.txt"
  dayTwoContent <- decodeUtf8 <$> readFileBS "inputs/day2.txt"
  dayThreeContent <- decodeUtf8 <$> readFileBS "inputs/day3.txt"
  dayFourContent <- decodeUtf8 <$> readFileBS "inputs/day4.txt"
  dayFiveContent <- decodeUtf8 <$> readFileBS "inputs/day5.txt"
  daySixContent <- decodeUtf8 <$> readFileBS "inputs/day6.txt"
  daySevenContent <- decodeUtf8 <$> readFileBS "inputs/day7.txt"
  dayEightContent <- decodeUtf8 <$> readFileBS "inputs/day8.txt"
  print ("day1pt1" ++ show (sumCalibrations dayOneContent))
  print ("day1pt2" ++ show (sumTextyCalis dayOneContent))
  print ("day2pt1" ++ show (sumPossibleGames standardBag dayTwoContent))
  print ("day2pt2" ++ show (sumPowers dayTwoContent))
  print ("day3pt1: " ++ show (day3pt1 dayThreeContent))
  print ("day3pt2: " ++ show (day3pt2 dayThreeContent))
  print ("day4pt1: " ++ show (day4pt1 dayFourContent))
  print ("day4pt2: " ++ show (day4pt2 dayFourContent))
  run "day5pt1: " (day5pt1 dayFiveContent)
  -- print ("day5pt2: " ++ show (day5pt2 dayFiveContent)) -- too slow and hot to run every time
  print ("day6pt1: " `append` day6pt1 daySixContent)
  print ("day6pt2: " `append` day6pt2 daySixContent)
  run "day7pt1" $ day7pt1 daySevenContent
  run "day7pt2" $ day7pt2 daySevenContent
  run "day8pt1" $ day8pt1 dayEightContent
  run "day8pt2" $ day8pt2 dayEightContent

run :: forall {m :: Type -> Type} {a}. (MonadIO m, Show a) => String -> m a -> m ()
run label f = do
  putStr (label ++ ": ")
  print =<< f
