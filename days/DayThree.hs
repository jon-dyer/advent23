{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- I like this lint in theory. but it offered in a supremely stupid place in isSymbolAdjacent
{-# HLINT ignore "Use <$>" #-}
module DayThree where

import Data.Char (digitToInt, isDigit)
import Data.List (maximum)
import Data.Map.Lazy (assocs)
import Data.Map.Strict (insert, lookup)
import Data.Sequence (Seq ((:|>)), mapWithIndex, (|>))
import Data.Sequence qualified as Seq
import Lib.Grid (Grid (..))

data NumberCoord where
  NumberCoord ::
    {row :: Int, colStart :: Int, colStop :: Int} ->
    NumberCoord
  deriving stock (Show, Eq)

data GearCoord where
  GearCoord :: Int -> Int -> GearCoord
  deriving stock (Show, Eq)

data Gritem where
  Gritem :: Int -> Int -> Datum -> Gritem

data Datum where
  Digit :: Int -> Datum
  Symbol :: Char -> Datum
  Empty :: Datum
  deriving stock (Show, Eq)

toInt :: Datum -> Maybe Int
toInt (Digit d) = Just d
toInt _ = Nothing

isSymbol :: Datum -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isSymbolOf :: Char -> Datum -> Bool
isSymbolOf c (Symbol s)
  | c == s = True
  | otherwise = False
isSymbolOf _ _ = False

e :: Int -> Int -> Int
e x y = x * (10 ^ y)

getNumber :: Grid Datum -> NumberCoord -> Maybe Int
getNumber g (NumberCoord r ca cb) =
  let ms :: Maybe [Int]
      ms = traverse (toInt . gridAt g r) [ca .. cb]
      listToInt :: [Int] -> Int
      listToInt =
        sum . (uncurry (flip e) <$>) . zip [0 ..] . reverse
   in listToInt <$> ms

gridAt :: Grid Datum -> Int -> Int -> Datum
gridAt (Grid _ _ ps) x y = fromMaybe Empty (lookup (x, y) ps)

consumeSchematic :: Text -> Grid Datum
consumeSchematic t =
  let ls :: [Text]
      ls = lines t
      gs :: Seq Gritem
      gs =
        join $
          mapWithIndex
            ( \r (l :: [Char]) ->
                mapWithIndex
                  ( \col (c :: Char) ->
                      Gritem
                        r
                        col
                        ( case () of
                            ()
                              | isDigit c -> Digit (digitToInt c)
                              | c == '.' -> Empty
                              | True -> Symbol c
                        )
                  )
                  (fromList l)
            )
            (fromList (toString <$> ls))
      xb :: Int
      xb = maximum ((\(Gritem x _ _) -> x) <$> gs)
      yb = maximum ((\(Gritem _ y _) -> y) <$> gs)
      gshm =
        foldr (\(Gritem x y i) soFar -> insert (x, y) i soFar) mempty gs
   in Grid xb yb gshm

findNumbers :: Grid Datum -> [NumberCoord]
findNumbers (Grid rows cols ps) =
  let sr :: [((Int, Int), Datum)]
      sr =
        do
          r <- [0 .. rows]
          c <- [0 .. cols]
          let coords = (r, c)
          return (coords, fromMaybe Empty (lookup coords ps))
      myCoords :: (Seq NumberCoord, Datum)
      myCoords =
        foldr
          theFold
          (Seq.Empty, Empty)
          sr
   in reverse $ toList $ fst myCoords

theFold :: ((Int, Int), Datum) -> (Seq NumberCoord, Datum) -> (Seq NumberCoord, Datum)
theFold (_, Empty) (sf, Empty) = (sf, Empty)
theFold ((r, c), Digit d) (Seq.Empty, _) =
  (NumberCoord r c c Seq.<| Seq.Empty, Digit d)
theFold ((r, c), Digit d) (res, prev) =
  case (res, prev) of
    (begin :|> (NumberCoord oldR _ oldCS), Digit _) ->
      let updatedCoord = NumberCoord oldR c oldCS
       in (begin |> updatedCoord, Digit d)
    (_, _) -> (res |> NumberCoord r c c, Digit d)
theFold (_, datum) (res, _) =
  (res, datum)

isSymbolAdjacent :: Grid Datum -> NumberCoord -> Bool
isSymbolAdjacent g (NumberCoord r cmin cmax) =
  let (Grid mr mc _) = g
      minRow = max 0 (r - 1)
      maxRow = min mr (r + 1)
      minCol = max 0 (cmin - 1)
      maxCol = min mc (cmax + 1)
      ds =
        do
          rs <- [minRow .. maxRow]
          cs <- [minCol .. maxCol]
          return $ gridAt g rs cs
   in any isSymbol ds

symbolAdjacentNumbers :: Grid Datum -> [Int]
symbolAdjacentNumbers g = mapMaybe (getNumber g) (filter (isSymbolAdjacent g) (findNumbers g))

day3pt1 :: Text -> Int
day3pt1 = sum . symbolAdjacentNumbers . consumeSchematic

findPotentialGear :: Grid Datum -> [GearCoord]
findPotentialGear grid =
  let pointies :: [((Int, Int), Datum)]
      pointies = assocs $ points grid
      symbols :: [((Int, Int), Datum)]
      symbols = filter (isSymbolOf '*' . snd) pointies
      coords = uncurry GearCoord . fst <$> symbols
   in coords

findGearAttachments :: Grid Datum -> GearCoord -> [NumberCoord]
findGearAttachments g (GearCoord sr sc) =
  filter
    ( \(NumberCoord r cSa cSo) ->
        let tlr = r - 1
            tlc = cSa - 1
            brr = r + 1
            brc = cSo + 1
         in sr >= tlr && sr <= brr && sc >= tlc && sc <= brc
    )
    (findNumbers g)

day3pt2 :: Text -> Int
day3pt2 t =
  let g = consumeSchematic t
      attachments = findGearAttachments g <$> findPotentialGear g
      actualGears = filter ((> 1) . length) attachments
      products = map (product . mapMaybe (getNumber g)) actualGears
   in sum products
