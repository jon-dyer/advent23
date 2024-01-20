{-# LANGUAGE OverloadedLists #-}

module DayEightSpec (day8) where

import DayEight (Direction (..), day8pt1, day8pt2, parseIt)
import Test.Tasty
import Test.Tasty.HUnit

day8 :: TestTree
day8 =
  let testData :: Text
      testData =
        toText
          ( "LLR\n\
            \\n\
            \AAA = (BBB, BBB)\n\
            \BBB = (AAA, ZZZ)\n\
            \ZZZ = (ZZZ, ZZZ)" ::
              String
          )
      testData1 :: Text
      testData1 =
        toText
          ( "RL\n\
            \\n\
            \AAA = (BBB, CCC)\n\
            \BBB = (DDD, EEE)\n\
            \CCC = (ZZZ, GGG)\n\
            \DDD = (DDD, DDD)\n\
            \EEE = (EEE, EEE)\n\
            \GGG = (GGG, GGG)\n\
            \ZZZ = (ZZZ, ZZZ)" ::
              String
          )
      testData3 :: Text
      testData3 =
        toText
          ( "LR\n\
            \n\
            \11A = (11B, XXX)\n\
            \11B = (XXX, 11Z)\n\
            \11Z = (11B, XXX)\n\
            \22A = (22B, XXX)\n\
            \22B = (22C, 22C)\n\
            \22C = (22Z, 22Z)\n\
            \22Z = (22B, 22B)\n\
            \XXX = (XXX, XXX)" ::
              String
          )
   in testGroup
        "day 8"
        [ testGroup
            "pt 1"
            [ testCase "all the way through" $
                do
                  res <- day8pt1 testData
                  res @?= "6",
              testCase "all the way through" $
                do
                  res <- day8pt1 testData1
                  res @?= "2",
              testCase "parse it" $
                parseIt testData
                  @?= Right
                    ( [L, L, R],
                      [ ("AAA", ("BBB", "BBB")),
                        ("BBB", ("AAA", "ZZZ")),
                        ("ZZZ", ("ZZZ", "ZZZ"))
                      ]
                    ),
              testCase "stay right" $
                do
                  dayEightContent <- decodeUtf8 <$> readFileBS "./inputs/day8.txt"
                  res <- day8pt1 dayEightContent
                  res @?= show (21251 :: Int)
            ],
          testGroup
            "pt 2"
            [ testCase "stay right" $
                do
                  dayEightContent <- decodeUtf8 <$> readFileBS "./inputs/day8.txt"
                  res <- day8pt2 dayEightContent
                  res @?= show (11_678_319_315_857 :: Int),
              testCase "all the way through" $
                do
                  res <- day8pt2 testData
                  res @?= "6",
              testCase "all the way through" $
                do
                  res <- day8pt2 testData1
                  res @?= "2"
            ]
        ]
