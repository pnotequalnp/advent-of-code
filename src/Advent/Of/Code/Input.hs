module Advent.Of.Code.Input
  ( list,
    nonEmpty,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

nonEmpty :: Read a => Text -> NonEmpty a
nonEmpty = NE.fromList . list

list :: Read a => Text -> [a]
list = fmap (read . T.unpack) . T.lines
