import Data.Char
import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Text.Read
import Data.Text.Read qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

{-
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
-}

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

readCalibration :: Text -> Int
readCalibration t =
  let firstPicker x = Char.digitToInt (Text.head (Text.dropWhile (not . isDigit) x))
   in (10 * firstPicker t) + firstPicker (Text.reverse t)

readCalibrations :: Text -> [Int]
readCalibrations t =
  map readCalibration (lines t)

sumCalibrations :: Text -> Int
sumCalibrations = sum . readCalibrations

unitTests :: TestTree
unitTests =
  let testData :: Text
      testData =
        toText
          ( "1abc2\n\
            \pqr3stu8vwx\n\
            \a1b2c3d4e5f\n\
            \treb7uchet" ::
              String
          )
   in testGroup
        "day 1"
        [ testCase "can do a thing" $
            readCalibration "pqr3stu8vwx" @?= 38,
          testCase "can do all lines" $
            readCalibrations testData @?= [12, 38, 15, 77],
          testCase "can sum all" $
            sumCalibrations testData @?= 142
        ]
