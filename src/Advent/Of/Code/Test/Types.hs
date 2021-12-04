module Advent.Of.Code.Test.Types
  ( Test (..),
    Tests,
  )
where

import Advent (Day, Part)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

type Tests = Map (Day, Part) [Test]

data Test = Test
  { input :: Text,
    output :: Text
  }
  deriving (Eq, Generic, Show)
