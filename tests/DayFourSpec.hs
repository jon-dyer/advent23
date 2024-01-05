module DayFourSpec (day4) where

import DayFour (Game (..), day4pt1, day4pt2, parseGame, parseGames, wins)
import Test.Tasty
import Test.Tasty.HUnit

day4 :: TestTree
day4 =
  let testData :: Text
      testData =
        toText
          ( "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
            \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
            \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
            \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
            \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
            \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" ::
              String
          )
   in testGroup
        "day 4"
        [ testGroup
            "pt 1"
            [ testCase "all the way through" $
                day4pt1 testData @?= 13,
              testCase "parseFirstLine" $
                parseGame "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                  @?= Right (Game [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53])
            ],
          testGroup
            "pt 2"
            [ testCase "all the way through" $
                day4pt2 testData
                  @?= 30,
              testCase "wins are right" $
                map wins (parseGames testData) @?= [4, 2, 2, 1, 0, 0]
            ]
        ]
