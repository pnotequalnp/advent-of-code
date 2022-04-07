-- |
-- Module      : Advent.Of.Code.Test
-- Description : Advent of Code solution tests
-- Copyright   : Kevin Mullins 2021-2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
-- Portability : portable
--
-- = Advent.Of.Code.Test
-- Generation of HUnit tests for Advent of Code solutions given some example input/output pairs.
module Advent.Of.Code.Test
  ( Test (..),
    Tests,
    makeTests,
  )
where

import Advent (Day, Part, dayInt, partInt)
import Advent.Of.Code.Test.Types (Test (..), Tests)
import Data.Map qualified as Map
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

type Solutions = Day -> Part -> Maybe Solution

type Solution = Text -> Text

-- | Generate tests for the given solutions and examples.
makeTests :: (Day -> Part -> Maybe (Text -> Text)) -> Tests -> TestTree
makeTests solutions = testGroup "advent" . Map.foldMapWithKey (pure .: makeDayTests solutions)
  where
    (.:) = (.) . (.)

makeDayTests :: Solutions -> (Day, Part) -> [Test] -> TestTree
makeDayTests solutions (day, part) tests =
  testGroup
    (show (dayInt day) <> "/" <> show (partInt part))
    case solutions day part of
      Nothing -> []
      Just solve -> zipWith (test solve) [0..] tests

test :: Solution -> Int -> Test -> TestTree
test solve n Test {input, output} = testCase (show n) (solve input @?= output)
