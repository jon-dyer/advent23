module DayFiveSpec (day5) where

import DayFive (day5pt1)
import Test.Tasty
import Test.Tasty.HUnit

day5 :: TestTree
day5 =
  let testData :: Text
      testData =
        toText
          ( "seeds: 79 14 55 13\n\
            \n\
            \seed-to-soil map:\n\
            \50 98 2\n\
            \52 50 48\n\
            \n\
            \soil-to-fertilizer map:\n\
            \0 15 37\n\
            \37 52 2\n\
            \39 0 15\n\
            \n\
            \fertilizer-to-water map:\n\
            \49 53 8\n\
            \0 11 42\n\
            \42 0 7\n\
            \57 7 4\n\
            \n\
            \water-to-light map:\n\
            \88 18 7\n\
            \18 25 70\n\
            \n\
            \light-to-temperature map:\n\
            \45 77 23\n\
            \81 45 19\n\
            \68 64 13\n\
            \n\
            \temperature-to-humidity map:\n\
            \0 69 1\n\
            \1 0 69\n\
            \n\
            \humidity-to-location map:\n\
            \60 56 37\n\
            \56 93 4" ::
              String
          )
   in testGroup
        "day 5"
        [ testGroup
            "pt 1"
            [ testCase "all the way through" $
                day5pt1 testData @?= 35
            ],
          testGroup
            "pt 2"
            []
        ]
