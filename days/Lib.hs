module Lib where

import Data.List.NonEmpty qualified as NE

mytrace :: forall {a}. (Show a) => [Char] -> a -> a
mytrace s a = trace (s ++ show a) a

wholeNumbers :: NonEmpty Int
wholeNumbers = NE.iterate (1 +) 0
