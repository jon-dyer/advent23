import DayOne (parseFirstNumber, parseLastNumber, readCalibration, readCalibrations, readTextyCali, readTextyCalis, sumCalibrations, sumTextyCalis)
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
unitTests =
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
