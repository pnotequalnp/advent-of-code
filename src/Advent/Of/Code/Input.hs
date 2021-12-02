module Advent.Of.Code.Input
  ( customList,
    customList',
    customNonEmpty,
    customNonEmpty',
    list,
    nonEmpty,
    plainList,
    plainNonEmpty,
    wordsList,
    wordsList',
    wordsNonEmpty,
    wordsNonEmpty',
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

plainNonEmpty :: Text -> NonEmpty Text
plainNonEmpty = NE.fromList . plainList

plainList :: Text -> [Text]
plainList = customList id

nonEmpty :: Read a => Text -> NonEmpty a
nonEmpty = NE.fromList . list

list :: Read a => Text -> [a]
list = fmap (read . T.unpack) . T.lines

customNonEmpty :: (Text -> a) -> Text -> NonEmpty a
customNonEmpty f = NE.fromList . customList f

customList :: (Text -> a) -> Text -> [a]
customList f = fmap f . T.lines

customNonEmpty' :: Applicative f => (Text -> f a) -> Text -> f (NonEmpty a)
customNonEmpty' f = fmap NE.fromList . customList' f

customList' :: Applicative f => (Text -> f a) -> Text -> f [a]
customList' f = traverse f . T.lines

wordsNonEmpty :: ([Text] -> a) -> Text -> NonEmpty a
wordsNonEmpty f = NE.fromList . wordsList f

wordsList :: ([Text] -> a) -> Text -> [a]
wordsList f = fmap (f . T.words) . T.lines

wordsNonEmpty' :: Applicative f => ([Text] -> f a) -> Text -> f (NonEmpty a)
wordsNonEmpty' f = fmap NE.fromList . wordsList' f

wordsList' :: Applicative f => ([Text] -> f a) -> Text -> f [a]
wordsList' f = traverse (f . T.words) . T.lines
