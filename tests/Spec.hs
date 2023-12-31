import DayOne (parseFirstNumber, parseLastNumber, readCalibration, readCalibrations, readTextyCali, readTextyCalis, sumCalibrations, sumTextyCalis)
import DayTwo (Bag (..), Cubes (..), Game (..), GamePossible (..), Pull (..), gamePossible, parseLine, power, pullPossible, smallestBag, standardBag, sumPossibleGames, sumPowers)
import Test.Tasty
import Test.Tasty.HUnit

{-
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
-}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [{-properties, -} unitTests]

{-
properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps =
  testGroup
    "(checked by SmallCheck)"
    [ SC.testProperty "sort == sort . reverse" $
        \list -> sort (list :: [Int]) == sort (reverse list),
      SC.testProperty "Fermat's little theorem" $
        \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
      -- the following property does not hold
      SC.testProperty "Fermat's last theorem" $
        \x y z n ->
          (n :: Integer) >= 3 SC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]

qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $
        \list -> sort (list :: [Int]) == sort (reverse list),
      QC.testProperty "Fermat's little theorem" $
        \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
      -- the following property does not hold
      QC.testProperty "Fermat's last theorem" $
        \x y z n ->
          (n :: Integer) >= 3 QC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]
    -}

unitTests :: TestTree
unitTests = testGroup "days" [day1, day2]

day1 :: TestTree
day1 =
  testGroup
    "day 1"
    [ let testData :: Text
          testData =
            toText
              ( "1abc2\n\
                \pqr3stu8vwx\n\
                \a1b2c3d4e5f\n\
                \treb7uchet" ::
                  String
              )
       in testGroup
            "pt 1"
            [ testCase "can do a thing" $
                readCalibration "pqr3stu8vwx" @?= Right 38,
              testCase "can do all lines" $
                readCalibrations testData @?= [12, 38, 15, 77],
              testCase "can sum all" $
                sumCalibrations testData @?= 142
            ],
      let testData =
            toText
              ( "two1nine\n\
                \eightwothree\n\
                \abcone2threexyz\n\
                \xtwone3four\n\
                \4nineeightseven2\n\
                \zoneight234\n\
                \7pqrstsixteen" ::
                  String
              )
       in testGroup
            "pt 2"
            [ testCase "can parse all the numbers in a string" $
                parseFirstNumber "two" @?= Right 2,
              testCase "can find the first number ignoring junk" $
                parseFirstNumber "onthreeblue1two" @?= Right 3,
              testCase "can parse all the numbers in a string, edge cases like twone" $
                parseLastNumber "onthreeblue4twoney" @?= Right 1,
              testCase "can do a thing" $
                readTextyCali "two1nine" @?= Right 29,
              testCase "can do all lines" $
                readTextyCalis testData @?= [29, 83, 13, 24, 42, 14, 76],
              testCase "can sum all" $
                sumTextyCalis testData @?= 281
            ]
    ]

day2 :: TestTree
day2 =
  let testData =
        toText
          ( "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
            \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
            \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
            \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
            \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ::
              String
          )
   in testGroup
        "day 2"
        [ let lineOne = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
           in testGroup
                "pt 1"
                [ testCase "turn a line into data" $
                    DayTwo.parseLine lineOne
                      @?= Right
                        Game
                          { ident = 1,
                            pulls =
                              [ Pull Cubes {blue = 3, red = 4, green = 0},
                                Pull Cubes {blue = 6, red = 1, green = 2},
                                Pull Cubes {blue = 0, red = 0, green = 2}
                              ]
                          },
                  testCase "is pull possible?" $
                    pullPossible standardBag
                      <$> [ Pull Cubes {red = 13, green = 0, blue = 0},
                            Pull Cubes {red = 0, green = 14, blue = 0},
                            Pull Cubes {red = 0, green = 0, blue = 15},
                            Pull Cubes {red = 12, green = 13, blue = 14}
                          ]
                      @?= [Impossible, Impossible, Impossible, Possible],
                  testCase "is game possible?" $
                    gamePossible standardBag
                      <$> [ Game {ident = 1, pulls = [Pull Cubes {red = 13, green = 0, blue = 0}]},
                            Game {ident = 1, pulls = [Pull Cubes {red = 0, green = 14, blue = 0}]},
                            Game {ident = 1, pulls = [Pull Cubes {red = 0, green = 0, blue = 15}]},
                            Game {ident = 1, pulls = [Pull Cubes {red = 12, green = 13, blue = 14}]},
                            Game
                              { ident = 1,
                                pulls =
                                  [ Pull Cubes {red = 12, green = 13, blue = 14},
                                    Pull Cubes {red = 0, green = 0, blue = 0}
                                  ]
                              },
                            Game
                              { ident = 1,
                                pulls =
                                  [ Pull Cubes {red = 12, green = 13, blue = 14},
                                    Pull Cubes {red = 0, green = 0, blue = 15}
                                  ]
                              }
                          ]
                      @?= [Impossible, Impossible, Impossible, Possible, Possible, Impossible],
                  testCase "can sum game IDs correctly" $
                    sumPossibleGames standardBag testData @?= 8
                ],
          testGroup
            "pt 2"
            [ testCase "find smallest possible" $
                smallestBag <$> rights (parseLine <$> lines testData)
                  @?= [ Bag Cubes {red = 4, green = 2, blue = 6},
                        Bag Cubes {red = 1, green = 3, blue = 4},
                        Bag Cubes {red = 20, green = 13, blue = 6},
                        Bag Cubes {red = 14, green = 3, blue = 15},
                        Bag Cubes {red = 6, green = 3, blue = 2}
                      ],
              testCase "find powers" $
                power . smallestBag
                  <$> rights (parseLine <$> lines testData)
                  @?= [ 48,
                        12,
                        1560,
                        630,
                        36
                      ],
              testCase "the sum, for completeness" $
                sumPowers testData @?= 2286
            ]
        ]
