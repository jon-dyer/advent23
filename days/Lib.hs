module Lib where

mytrace :: forall {a}. (Show a) => [Char] -> a -> a
mytrace s a = trace (s ++ show a) a
