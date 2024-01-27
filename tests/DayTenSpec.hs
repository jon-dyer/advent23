module DayTenSpec (day10) where

import DayTen (day10pt1)
import Test.Tasty
import Test.Tasty.HUnit

day10 :: TestTree
day10 =
  let testData :: Text
      testData =
        toText
          ( ".....\n\
            \.F-7.\n\
            \.|.|.\n\
            \.L-J.\n\
            \....." ::
              String
          )
   in testGroup
        "day 10"
        [ testGroup
            "pt 1"
            [ testCase "all the way through" $
                do
                  res <- day10pt1 testData
                  res @?= "4",
              testCase "stay correct" $
                do
                  dayNineContent <- decodeUtf8 <$> readFileBS "./inputs/day10.txt"
                  res <- day10pt1 dayNineContent
                  res @?= show (0 :: Int)
            ],
          testGroup
            "pt 2"
            []
        ]

{- testCase "simple example" $
    do
      lastLine <- evaluateWHNF $ viaNonEmpty head (reverse $ lines testData)
      res <- day10pt2 `traverse` lastLine
      res @?= Just "5",
  testCase "stay correct" $
    do
      dayNineContent <- decodeUtf8 <$> readFileBS "./inputs/day9.txt"
      res <- day9pt2 dayNineContent
      res @?= show (1026 :: Int)
-}
