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

parseTests :: Text -> Either [TomlDecodeError] Tests
parseTests = decode testsCodec

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
