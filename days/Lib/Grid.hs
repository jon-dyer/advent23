module Lib.Grid where

data Grid a where
  Grid ::
    { xSize :: Int,
      ySize :: Int,
      points :: Map (Int, Int) a
    } ->
    Grid a
  deriving stock (Show, Eq)

defaultGrid :: Grid a
defaultGrid =
  Grid 0 0 mempty
