module DayThreeSpec (day3) where

import DayThree (Datum (..), GearCoord (..), NumberCoord (..), consumeSchematic, day3pt1, day3pt2, findGearAttachments, findNumbers, findPotentialGear, getNumber, gridAt, isSymbolAdjacent, symbolAdjacentNumbers)
import Test.Tasty
import Test.Tasty.HUnit

day3 :: TestTree
day3 =
  let testData :: Text
      testData =
        toText
          ( "467..114..\n\
            \...*......\n\
            \..35..633.\n\
            \......#...\n\
            \617*......\n\
            \.....+.58.\n\
            \..592.....\n\
            \......755.\n\
            \...$.*....\n\
            \.664.598.." ::
              String
          )
   in testGroup
        "day 3"
        [ testGroup
            "pt 1"
            [ testCase "sums all the things next to a symbol" $
                day3pt1 testData @?= 4361,
              testCase "sums all the things next to a symbol" $
                let subject = consumeSchematic testData
                 in sum (symbolAdjacentNumbers subject) @?= 4361,
              testCase "can retriev chars numbers" $
                let subject = consumeSchematic testData
                 in gridAt subject 0 0 @?= Digit 4,
              testCase "retrieve a number" $
                let subject = consumeSchematic testData
                 in getNumber subject (NumberCoord 0 0 2) @?= Just 467,
              testCase "find numbers" $
                let subject = consumeSchematic testData
                 in findNumbers subject
                      @?= [ NumberCoord 0 0 2,
                            NumberCoord 0 5 7,
                            NumberCoord 2 2 3,
                            NumberCoord 2 6 8,
                            NumberCoord 4 0 2,
                            NumberCoord 5 7 8,
                            NumberCoord 6 2 4,
                            NumberCoord 7 6 8,
                            NumberCoord 9 1 3,
                            NumberCoord 9 5 7
                          ],
              testCase "isNextToSymbol" $
                let subject = consumeSchematic testData
                 in isSymbolAdjacent subject
                      <$> [ NumberCoord 0 0 2,
                            NumberCoord 0 5 7,
                            NumberCoord 2 2 3,
                            NumberCoord 2 6 8,
                            NumberCoord 4 0 2,
                            NumberCoord 5 7 8,
                            NumberCoord 6 2 4,
                            NumberCoord 7 6 8,
                            NumberCoord 9 1 3,
                            NumberCoord 9 5 7
                          ]
                      @?= [True, False, True, True, True, False, True, True, True, True]
            ],
          testGroup
            "pt2"
            [ testCase "find potential coggers" $
                let subject = consumeSchematic testData
                 in findPotentialGear subject
                      @?= [ GearCoord 1 3,
                            GearCoord 4 3,
                            GearCoord 8 5
                          ],
              testCase "match with nums" $
                let grid = consumeSchematic testData
                 in findGearAttachments grid (GearCoord 1 3)
                      @?= [ NumberCoord 0 0 2,
                            NumberCoord 2 2 3
                          ],
              testCase "product all gear attachments w > 1 and sum" $
                day3pt2 testData @?= 467835
            ]
        ]
