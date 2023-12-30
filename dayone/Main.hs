module Main where

import DayOne (sumCalibrations, sumTextyCalis)

main :: IO ()
main = do
  content <- readFileText "inputs/day1.txt"
  print (show (sumCalibrations content))
  print (show (sumTextyCalis content))
