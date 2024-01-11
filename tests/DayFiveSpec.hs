module DayFiveSpec (day5) where

import DayFive (Range (..), Seed (..), SeedRange (..), day5pt1, day5pt2, dstToSrc, parseIt, parseItSeedRanges, seedLocation, srcToDst)
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
                      [ [ Range 50 51 98 99,
                          Range 52 99 50 97
                        ],
                        [ Range 0 36 15 51,
                          Range 37 38 52 53,
                          Range 39 53 0 14
                        ],
                        [ Range 49 56 53 60,
                          Range 0 41 11 52,
                          Range 42 48 0 6,
                          Range 57 60 7 10
                        ],
                        [ Range 88 94 18 24,
                          Range 18 87 25 94
                        ],
                        [ Range 45 67 77 99,
                          Range 81 99 45 63,
                          Range 68 80 64 76
                        ],
                        [ Range 0 0 69 69,
                          Range 1 69 0 68
                        ],
                        [ Range 60 96 56 92,
                          Range 56 59 93 96
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
                srcToDst 3 (Range 4 6 2 4) @?= 5,
              testCase "src2dst2" $
                srcToDst 5 (Range 2 4 4 6) @?= 3,
              testCase "src2dst3" $
                srcToDst 5 (Range 0 2 4 6) @?= 1,
              testCase "src2dst4" $
                srcToDst 1 (Range 2 4 0 2) @?= 3,
              testCase "src2dst5" $
                srcToDst 45 (Range 45 45 45 45) @?= 45
            ],
          testGroup
            "pt 2"
            [ testCase "the seed as ranges" $
                parseItSeedRanges testData
                  @?= Right
                    ( [SeedRange 79 92, SeedRange 55 67],
                      [ [ Range 50 51 98 99,
                          Range 52 99 50 97
                        ],
                        [ Range 0 36 15 51,
                          Range 37 38 52 53,
                          Range 39 53 0 14
                        ],
                        [ Range 49 56 53 60,
                          Range 0 41 11 52,
                          Range 42 48 0 6,
                          Range 57 60 7 10
                        ],
                        [ Range 88 94 18 24,
                          Range 18 87 25 94
                        ],
                        [ Range 45 67 77 99,
                          Range 81 99 45 63,
                          Range 68 80 64 76
                        ],
                        [ Range 0 0 69 69,
                          Range 1 69 0 68
                        ],
                        [ Range 60 96 56 92,
                          Range 56 59 93 96
                        ]
                      ]
                    ),
              testCase "the seed" $
                day5pt2 testData @?= 46,
              testCase "dst2src" $
                dstToSrc 5 (Range 4 6 2 4) @?= 3,
              testCase "dst2Src2" $
                dstToSrc 3 (Range 2 4 4 6) @?= 5,
              testCase "dst2src3" $
                dstToSrc 1 (Range 0 2 4 6) @?= 5,
              testCase "dst2src4" $
                dstToSrc 3 (Range 2 4 0 2) @?= 1,
              testCase "dst2src5" $
                dstToSrc 45 (Range 45 45 45 45) @?= 45
            ]
        ]
