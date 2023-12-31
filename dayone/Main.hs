module Main where

import DayOne (sumCalibrations, sumTextyCalis)
import DayTwo (standardBag, sumPossibleGames)

main :: IO ()
main = do
  dayOneContent <- readFileText "inputs/day1.txt"
  dayTwoContent <- readFileText "inputs/day2.txt"
  print (show (sumCalibrations dayOneContent))
  print (show (sumTextyCalis dayOneContent))
  print (show (sumPossibleGames standardBag dayTwoContent))
