module Lib where

import Data.List.NonEmpty qualified as NE

mytrace :: forall {a}. (Show a) => [Char] -> a -> a
mytrace s a = trace (s ++ show a) a

wholeNumbers :: NonEmpty Int
wholeNumbers = NE.iterate (1 +) 0

nel :: forall {f :: Type -> Type} {a}. (Alternative f) => f a -> f (NonEmpty a)
nel p = (:|) <$> p <*> many p
