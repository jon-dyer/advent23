module Main where

import DayOne (sumCalibrations, sumTextyCalis)
import DayThree (day3pt1)
import DayTwo (standardBag, sumPossibleGames, sumPowers)

main :: IO ()
main = do
  dayOneContent <- readFileText "inputs/day1.txt"
  dayTwoContent <- readFileText "inputs/day2.txt"
  dayThreeContent <- readFileText "inputs/day3.txt"
  print (show (sumCalibrations dayOneContent))
  print (show (sumTextyCalis dayOneContent))
  print (show (sumPossibleGames standardBag dayTwoContent))
  print (show (sumPowers dayTwoContent))
  print ("day3pt1: " ++ show (day3pt1 dayThreeContent))
