-- |
-- Module      : Advent.Of.Code.Test.Parsing
-- Description : Advent of Code solution test TOML parsing
-- Copyright   : Kevin Mullins 2021-2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
-- Portability : portable
--
-- = Advent.Of.Code.Test.Parsing
-- TOML parsing utilities for Advent of Code solution tests.
module Advent.Of.Code.Test.Parsing
  ( generateTests,
    parseTests,
    prettyTomlDecodeErrors,
  )
where

import Advent (Part (..), dayInt, mkDay, partInt)
import Advent.Of.Code.Test.Types (Tests)
import Control.Monad ((<=<))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)
import Toml hiding (day)

-- | Parse TOML into `Tests`.
parseTests :: Text -> Either [TomlDecodeError] Tests
parseTests = decode testsCodec

-- | Generate TOML for the given `Tests`.
generateTests :: Tests -> Text
generateTests = encode testsCodec

testsCodec :: TomlCodec Tests
testsCodec = tableMap (BiMap fromKey toKey) (list genericCodec) "test"
  where
    toKey (day, part) =
      Right $ Piece (T.pack . show $ dayInt day) :|| [Piece (T.pack . show $ partInt part)]
    fromKey =
      maybe (Left $ ArbitraryError "Couldn't parse day") Right . \case
        Piece ((mkDay <=< readMaybe) . T.unpack -> Just day)
          :|| [Piece ((mkPart <=< readMaybe) . T.unpack -> Just part)] -> Just (day, part)
        _ -> Nothing

mkPart :: Int -> Maybe Part
mkPart = \case
  1 -> Just Part1
  2 -> Just Part2
  _ -> Nothing
