module DayNineSpec (day9) where

import DayNine (day9pt1, day9pt2)
import Test.Tasty
import Test.Tasty.HUnit

day9 :: TestTree
day9 =
  let testData :: Text
      testData =
        toText
          ( "0 3 6 9 12 15\n\
            \1 3 6 10 15 21\n\
            \10 13 16 21 30 45" ::
              String
          )
   in testGroup
        "day 9"
        [ testGroup
            "pt 1"
            [ testCase "all the way through" $
                do
                  res <- day9pt1 testData
                  res @?= "114",
              testCase "simple example" $
                do
                  firstLine <- evaluateWHNF $ viaNonEmpty head (lines testData)
                  res <- day9pt1 `traverse` firstLine
                  res @?= Just "18",
              testCase "stay correct" $
                do
                  dayNineContent <- decodeUtf8 <$> readFileBS "./inputs/day9.txt"
                  res <- day9pt1 dayNineContent
                  res @?= show (1_584_748_274 :: Int)
            ],
          testGroup
            "pt 2"
            [ testCase "simple example" $
                do
                  lastLine <- evaluateWHNF $ viaNonEmpty head (reverse $ lines testData)
                  res <- day9pt2 `traverse` lastLine
                  res @?= Just "5",
              testCase "stay correct" $
                do
                  dayNineContent <- decodeUtf8 <$> readFileBS "./inputs/day9.txt"
                  res <- day9pt2 dayNineContent
                  res @?= show (1026 :: Int)
            ]
        ]
