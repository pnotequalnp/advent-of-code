module Test.Advent.Of.Code.Test.Parsing (generatingTests, parsingTests) where

import Advent (Part (..), mkDay_)
import Advent.Of.Code.Test.Parsing (generateTests, parseTests)
import Advent.Of.Code.Test.Types (Test (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

parsingTests :: TestTree
parsingTests =
  testGroup
    "parsing"
    [ oneTest,
      manyTests
    ]

generatingTests :: TestTree
generatingTests =
  testGroup
     "generating"
     [ oneTestGen
     ]

oneTest :: TestTree
oneTest = testCase "oneTest" $ parseTests input @?= Right expected
  where
    input =
      T.unlines
        [ "[test]",
          "[[test.7.2]]",
          "input = \"foo\"",
          "output = \"bar\""
        ]
    expected = Map.fromList [((mkDay_ 7, Part2), [Test "foo" "bar"])]

manyTests :: TestTree
manyTests = testCase "manyTests" $ parseTests input @?= Right expected
  where
    input =
      T.unlines
        [ "[test]",
          "[[test.7.2]]",
          "input = \"foo\"",
          "output = \"bar\"",
          "[[test.13.1]]",
          "input = \"foo\"",
          "output = \"bar\"",
          "[[test.13.1]]",
          "input = \"foo\"",
          "output = \"bar\"",
          "[[test.13.2]]",
          "input = \"foo\"",
          "output = \"bar\""
        ]
    expected =
      Map.fromList
        [ ((mkDay_ 7, Part2), [Test "foo" "bar"]),
          ((mkDay_ 13, Part1), [Test "foo" "bar", Test "foo" "bar"]),
          ((mkDay_ 13, Part2), [Test "foo" "bar"])
        ]

oneTestGen :: TestTree
oneTestGen = testCase "oneTest" $ generateTests tests @?= expected
  where
    tests = Map.fromList [((mkDay_ 7, Part2), [Test "arst" "foobar"])]
    expected =
      T.unlines
        [ "[[7.2]]",
          "input = \"arst\"",
          "output = \"foobar\""
        ]
