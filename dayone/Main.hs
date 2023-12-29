module Main where

import DayOne (sumCalibrations)

main :: IO ()
main = do
  content <- readFileText "inputs/day1.txt"
  print (show (sumCalibrations content))
