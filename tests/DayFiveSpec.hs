module DayFiveSpec (day5) where

import DayFive (Mapper (..), Range (..), Seed (..), day5pt1, parseIt, seedLocation, srcToDst)
import Test.Tasty
import Test.Tasty.HUnit

day5 :: TestTree
day5 =
  let testData :: Text
      testData =
        toText
          ( "seeds: 79 14 55 13\n\
            \\n\
            \seed-to-soil map:\n\
            \50 98 2\n\
            \52 50 48\n\
            \\n\
            \soil-to-fertilizer map:\n\
            \0 15 37\n\
            \37 52 2\n\
            \39 0 15\n\
            \\n\
            \fertilizer-to-water map:\n\
            \49 53 8\n\
            \0 11 42\n\
            \42 0 7\n\
            \57 7 4\n\
            \\n\
            \water-to-light map:\n\
            \88 18 7\n\
            \18 25 70\n\
            \\n\
            \light-to-temperature map:\n\
            \45 77 23\n\
            \81 45 19\n\
            \68 64 13\n\
            \\n\
            \temperature-to-humidity map:\n\
            \0 69 1\n\
            \1 0 69\n\
            \\n\
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
                day5pt1 testData @?= 35,
              testCase "the seed" $
                parseIt testData
                  @?= Right
                    ( Seed [79, 14, 55, 13],
                      [ [ SeedToSoil (Range 50 98 2),
                          SeedToSoil (Range 52 50 48)
                        ],
                        [ SoilToFertilizer (Range 0 15 37),
                          SoilToFertilizer (Range 37 52 2),
                          SoilToFertilizer (Range 39 0 15)
                        ],
                        [ FertilizerToWater (Range 49 53 8),
                          FertilizerToWater (Range 0 11 42),
                          FertilizerToWater (Range 42 0 7),
                          FertilizerToWater (Range 57 7 4)
                        ],
                        [ WaterToLight (Range 88 18 7),
                          WaterToLight (Range 18 25 70)
                        ],
                        [ LightToTemp (Range 45 77 23),
                          LightToTemp (Range 81 45 19),
                          LightToTemp (Range 68 64 13)
                        ],
                        [ TempToHumidity (Range 0 69 1),
                          TempToHumidity (Range 1 0 69)
                        ],
                        [ HumidityToLocation (Range 60 56 37),
                          HumidityToLocation (Range 56 93 4)
                        ]
                      ]
                    ),
              testCase "match a seed to a location first" $
                let (Right (_, ms)) = parseIt testData
                 in seedLocation ms 79 @?= 82,
              testCase "match a seed to a location last" $
                let (Right (_, ms)) = parseIt testData
                 in seedLocation ms 13 @?= 35,
              testCase "src2dst" $
                srcToDst 3 (Range 4 2 3) @?= 5,
              testCase "src2dst2" $
                srcToDst 5 (Range 2 4 3) @?= 3,
              testCase "src2dst3" $
                srcToDst 5 (Range 0 4 3) @?= 1,
              testCase "src2dst4" $
                srcToDst 1 (Range 2 0 3) @?= 3,
              testCase "src2dst5" $
                srcToDst 45 (Range 45 45 1) @?= 45
            ],
          testGroup
            "pt 2"
            []
        ]
