module Advent.Of.Code.Toml (toml) where

import Advent (Part (..), partInt)
import Control.Applicative (optional)
import Data.Char (isAlphaNum)
import Data.List (partition)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, manyTill, sepEndBy, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (char, eol, hspace1, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

toml :: Parser [(Part, [(String, Text, String)])]
toml = do
  _ <- vsc
  entries <- entry `sepEndBy` vsc
  let (part1s, part2s) = partition ((== Part1) . fst) (concat entries)
      part1 = case snd <$> part1s of
        [] -> []
        xs -> [(Part1, xs)]
      part2 = case snd <$> part2s of
        [] -> []
        xs -> [(Part2, xs)]
  pure (part1 <> part2)

entry :: Parser [(Part, (String, Text, String))]
entry = do
  _ <- symbol "["
  name <- lexeme (takeWhile1P Nothing isAlphaNum)
  _ <- symbol "]"
  _ <- vsc
  kvPairs <- kvPair `sepEndBy` vsc
  Just input <- pure (lookup "input" kvPairs)
  let outputs = do
        part <- [Part1, Part2]
        output <- maybeToList (lookup ("part" <> show (partInt part)) kvPairs)
        pure (part, (name, T.pack input, output))
  pure outputs

kvPair :: Parser (String, String)
kvPair = do
  key <- lexeme (takeWhile1P Nothing isAlphaNum)
  _ <- symbol "="
  value <- try multiString <|> simpleString
  pure (key, value)
  where
    simpleString = char '"' *> manyTill L.charLiteral (char '\"')
    multiString = do
      _ <- symbol "\"\"\""
      _ <- optional eol
      manyTill L.charLiteral (symbol "\"\"\"")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space hspace1 empty empty

vsc :: Parser ()
vsc = L.space space1 empty empty
