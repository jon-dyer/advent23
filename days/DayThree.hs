{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- I like this lint in theory. but it offered in a supremely stupid place in isSymbolAdjacent
{-# HLINT ignore "Use <$>" #-}
module DayThree where

import Data.Char (digitToInt, isDigit)
import Data.List (maximum)
import Data.List.NonEmpty (prependList)
import Data.Map.Strict (insert, lookup)
import Data.Sequence (mapWithIndex)
import Relude.Unsafe (read)

data Grid = Grid
  { xSize :: Int,
    ySize :: Int,
    points :: Map (Int, Int) Datum
  }
  deriving stock (Show, Eq)

data NumberCoord = NumberCoord
  { row :: Int,
    colStart :: Int,
    colStop :: Int
  }
  deriving stock (Show, Eq)

defaultGrid :: Grid
defaultGrid =
  Grid 0 0 mempty

data Gritem = Gritem Int Int Datum

data Datum = Digit Int | Symbol Char | Empty
  deriving stock (Show, Eq)

isSymbol :: Datum -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

getNumber :: Grid -> NumberCoord -> Maybe Int
getNumber g (NumberCoord r ca cb) =
  let ms :: [Datum]
      ms = map (gridAt g r) [ca .. cb]
      is :: Maybe Int
      is =
        read
          <$> foldr
            ( \val soFar -> case (val, soFar) of
                (_, Nothing) -> Nothing
                (Digit d, Just ds) -> Just (show d ++ ds)
                (_, _) -> Nothing
            )
            (Just "")
            ms
   in is

gridAt :: Grid -> Int -> Int -> Datum
gridAt (Grid _ _ ps) x y = fromMaybe Empty (lookup (x, y) ps)

consumeSchematic :: Text -> Grid
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

findNumbers :: Grid -> [NumberCoord]
findNumbers (Grid rows cols ps) =
  let sr :: [((Int, Int), Datum)]
      sr =
        do
          r <- [0 .. rows]
          c <- [0 .. cols]
          let coords = (r, c)
          return (coords, fromMaybe Empty (lookup coords ps))
      myCoords :: ([NumberCoord], Datum)
      myCoords =
        foldr
          ( \(((r, c), datum) :: ((Int, Int), Datum)) ((res, prev) :: ([NumberCoord], Datum)) ->
              case (datum, nonEmpty res, prev) of
                (Digit d, Nothing, Digit _) ->
                  ([NumberCoord r c c], Digit d)
                (Digit d, Just n, Digit _) ->
                  let (NumberCoord oldR _ oldCS) = last n
                      updatedCoord = NumberCoord oldR c oldCS
                   in (toList $ prependList (init n) (updatedCoord :| []), Digit d)
                (Digit d, _, _) -> (reverse (NumberCoord r c c : reverse res), Digit d)
                (curr, _, _) -> (res, curr)
          )
          ([], Empty)
          sr
   in reverse $ fst myCoords

isSymbolAdjacent :: Grid -> NumberCoord -> Bool
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

symbolAdjacentNumbers :: Grid -> [Int]
symbolAdjacentNumbers g = mapMaybe (getNumber g) (filter (isSymbolAdjacent g) (findNumbers g))

day3pt1 :: Text -> Int
day3pt1 = sum . symbolAdjacentNumbers . consumeSchematic
