-- File: test/Main.hs

module Main where

import Test.Hspec
-- Import your spec module here
import qualified TeamSpeak.ParserSpec -- Use qualified import to avoid naming conflicts

main :: IO ()
main = hspec $ do
  -- Run the tests defined in TeamSpeak.ParserSpec
  TeamSpeak.ParserSpec.spec
  -- You can add other spec modules here if you create them later
  -- otherSpec.spec