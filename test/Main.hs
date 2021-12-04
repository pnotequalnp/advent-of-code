module Main (main) where

import Test.Advent.Of.Code.Test.Parsing (parsingTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "advent-of-code" tests)

tests :: [TestTree]
tests =
  [parsingTests
  ]
