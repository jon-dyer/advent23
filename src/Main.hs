module Main where

import DayFour (day4pt1, day4pt2)
import DayOne (sumCalibrations, sumTextyCalis)
import DayThree (day3pt1, day3pt2)
import DayTwo (standardBag, sumPossibleGames, sumPowers)

main :: IO ()
main = do
  dayOneContent <- decodeUtf8 <$> readFileBS "inputs/day1.txt"
  dayTwoContent <- decodeUtf8 <$> readFileBS "inputs/day2.txt"
  dayThreeContent <- decodeUtf8 <$> readFileBS "inputs/day3.txt"
  dayFourContent <- decodeUtf8 <$> readFileBS "inputs/day4.txt"
  print ("day1pt1" ++ show (sumCalibrations dayOneContent))
  print ("day1pt2" ++ show (sumTextyCalis dayOneContent))
  print ("day2pt1" ++ show (sumPossibleGames standardBag dayTwoContent))
  print ("day2pt2" ++ show (sumPowers dayTwoContent))
  print ("day3pt1: " ++ show (day3pt1 dayThreeContent))
  print ("day3pt2: " ++ show (day3pt2 dayThreeContent))
  print ("day4pt1: " ++ show (day4pt1 dayFourContent))
  print ("day4pt2: " ++ show (day4pt2 dayFourContent))
