module Main where

import DayOne (sumCalibrations, sumTextyCalis)

main :: IO ()
main = do
  dayOneContent <- readFileText "inputs/day1.txt"
  print (show (sumCalibrations dayOneContent))
  print (show (sumTextyCalis dayOneContent))
