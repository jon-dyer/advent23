module DaySixSpec (day6) where

import DaySix (Race (..), day6pt1, day6pt2, parseIt, parseSpaced)
import Test.Tasty
import Test.Tasty.HUnit

day6 :: TestTree
day6 =
  let testData :: Text
      testData =
        toText
          ( "Time:      7  15   30\n\
            \Distance:  9  40  200" ::
              String
          )
   in testGroup
        "day 6"
        [ testGroup
            "pt 1"
            [ testCase "all the way through" $
                day6pt1 testData @?= "288",
              testCase "parseIt" $
                parseIt testData
                  @?= Right
                    [ Race 7 9,
                      Race 15 40,
                      Race 30 200
                    ]
            ],
          testGroup
            "pt 2"
            [ testCase "all the way through" $
                day6pt2 testData @?= "71503",
              testCase "parse pt 2" $
                parseSpaced testData @?= Right (Race 71530 940200)
            ]
        ]
