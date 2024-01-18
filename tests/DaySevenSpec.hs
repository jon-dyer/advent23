module DaySevenSpec (day7) where

import DaySeven (Bid (..), Card (..), Hand (..), Kind (..), Rule (..), categorize, day7pt1, day7pt2, parseIt, sortEm)
import Test.Tasty
import Test.Tasty.HUnit

day7 :: TestTree
day7 =
  let testData :: Text
      testData =
        toText
          ( "32T3K 765\n\
            \T55J5 684\n\
            \KK677 28\n\
            \KTJJT 220\n\
            \QQQJA 483" ::
              String
          )
   in testGroup
        "day 7"
        [ testGroup
            "pt 1"
            [ testCase "all the way through" $
                do
                  res <- day7pt1 testData
                  res @?= "6440",
              testCase "parseIt" $
                parseIt Jack testData
                  @?= Right
                    [ (Hand [Three, Two, T, Three, K], Bid 765),
                      (Hand [T, Five, Five, Ja, Five], Bid 684),
                      (Hand [K, K, Six, Seven, Seven], Bid 28),
                      (Hand [K, T, Ja, Ja, T], Bid 220),
                      (Hand [Q, Q, Q, Ja, A], Bid 483)
                    ],
              testCase "categorize" $
                let thing = parseIt Jack testData
                 in (categorize . fst <$>) <$> thing
                      @?= Right
                        [ Pair,
                          ThreeOf,
                          TwoPair,
                          TwoPair,
                          ThreeOf
                        ],
              testCase "sortEm actually right" $
                let thing = parseIt Jack testData
                 in sortEm <$> thing
                      @?= Right
                        [ (Hand [Three, Two, T, Three, K], Bid 765),
                          (Hand [K, T, Ja, Ja, T], Bid 220),
                          (Hand [K, K, Six, Seven, Seven], Bid 28),
                          (Hand [T, Five, Five, Ja, Five], Bid 684),
                          (Hand [Q, Q, Q, Ja, A], Bid 483)
                        ],
              testCase "stay right" $
                do
                  daySevenContent <- decodeUtf8 <$> readFileBS "inputs/day7.txt"
                  res <- day7pt1 daySevenContent
                  res @?= show (249_483_956 :: Int)
            ],
          testGroup
            "pt 2"
            [ testCase "all the way through" $
                do
                  res <- day7pt2 testData
                  res @?= "5905",
              testCase "stay right" $
                do
                  daySevenContent <- decodeUtf8 <$> readFileBS "inputs/day7.txt"
                  res <- day7pt2 daySevenContent
                  res @?= show (252_137_472 :: Int)
            ]
        ]
