-- |
-- Module      : Advent.Of.Code.Input
-- Description : Advent of Code input convenience functions
-- Copyright   : Kevin Mullins 2021-2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
-- Portability : portable
--
-- = Advent.Of.Code.Input
-- Basic convenience functions for manipulating AoC inputs.
module Advent.Of.Code.Input
  ( -- * Lists
    list,
    plainList,
    wordsList,
    customList,

    -- ** Effectful
    wordsList',
    customList',

    -- * Non-Empty Lists
    nonEmpty,
    plainNonEmpty,
    wordsNonEmpty,
    customNonEmpty,

    -- ** Effectful
    wordsNonEmpty',
    customNonEmpty',

    -- * Miscellaneous
    readt,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

-- | Parse a @Text@ value using a @Show@ instance.
readt :: Read a => Text -> a
readt = read . T.unpack

-- | Break the input into lines.
plainNonEmpty :: Text -> NonEmpty Text
plainNonEmpty = NE.fromList . plainList

-- | Break the input into lines.
plainList :: Text -> [Text]
plainList = customList id

-- | Break the input into lines and parse them using a @Show@ instance.
nonEmpty :: Read a => Text -> NonEmpty a
nonEmpty = NE.fromList . list

-- | Break the input into lines and parse them using a @Show@ instance.
list :: Read a => Text -> [a]
list = fmap (read . T.unpack) . T.lines

-- | Break the input into lines and parse them using a provided function.
customNonEmpty :: (Text -> a) -> Text -> NonEmpty a
customNonEmpty f = NE.fromList . customList f

-- | Break the input into lines and parse them using a provided function.
customList :: (Text -> a) -> Text -> [a]
customList f = fmap f . T.lines

-- | Break the input into lines and parse them using a provided effectful parser.
customNonEmpty' :: Applicative f => (Text -> f a) -> Text -> f (NonEmpty a)
customNonEmpty' f = fmap NE.fromList . customList' f

-- | Break the input into lines and parse them using a provided effectful parser.
customList' :: Applicative f => (Text -> f a) -> Text -> f [a]
customList' f = traverse f . T.lines

-- | Break the input into lines and parse them by words using a provided function.
wordsNonEmpty :: ([Text] -> a) -> Text -> NonEmpty a
wordsNonEmpty f = NE.fromList . wordsList f

-- | Break the input into lines and parse them by words using a provided function.
wordsList :: ([Text] -> a) -> Text -> [a]
wordsList f = fmap (f . T.words) . T.lines

-- | Break the input into lines and parse them by words using a provided effectful parser.
wordsNonEmpty' :: Applicative f => ([Text] -> f a) -> Text -> f (NonEmpty a)
wordsNonEmpty' f = fmap NE.fromList . wordsList' f

-- | Break the input into lines and parse them by words using a provided effectful parser.
wordsList' :: Applicative f => ([Text] -> f a) -> Text -> f [a]
wordsList' f = traverse (f . T.words) . T.lines
