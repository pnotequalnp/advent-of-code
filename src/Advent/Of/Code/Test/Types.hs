-- |
-- Module      : Advent.Of.Code.Test.Types
-- Description : Advent of Code solution test types
-- Copyright   : Kevin Mullins 2021-2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
-- Portability : portable
--
-- = Advent.Of.Code.Test.Types
-- Types for constructing tests of solutions.
module Advent.Of.Code.Test.Types
  ( Test (..),
    Tests,
  )
where

import Advent (Day, Part)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A set of tests, organized by day and part.
type Tests = Map (Day, Part) [Test]

-- | A single test, consisting of a given input and expected output.
data Test = Test
  { input :: Text,
    output :: Text
  }
  deriving (Eq, Generic, Show)
