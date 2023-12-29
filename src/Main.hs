module Main where

import Main.Utf8 qualified as Utf8

-- |
-- Main entry point.
--
-- The `, run` script will invoke this function.
main :: IO ()
main = do
  Utf8.withUtf8 $ do
    putTextLn "Hello 🌎"
