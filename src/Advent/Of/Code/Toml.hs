module Advent.Of.Code.Toml where

import Advent (Part (..))
import Control.Applicative (optional)
import Data.Char (isAlphaNum)
import Data.List (partition)
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, manyTill, sepEndBy, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (char, eol, hspace1, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

toml :: Parser [[(String, String, String)]]
toml = do
  _ <- vsc
  entries <- entry `sepEndBy` vsc
  let (part1s, part2s) = partition (\(p, _, _, _) -> p == Part1) entries
      dropFst (_, x, y, z) = (x, y, z)
  pure [dropFst <$> part1s, dropFst <$> part2s]

entry :: Parser (Part, String, String, String)
entry = do
  _ <- symbol "["
  part <- Part1 <$ symbol "1" <|> Part2 <$ symbol "2"
  _ <- symbol "."
  name <- lexeme (takeWhile1P Nothing isAlphaNum)
  _ <- symbol "]"
  _ <- vsc
  kvPairs <- kvPair `sepEndBy` vsc
  Just input <- pure (lookup "input" kvPairs)
  Just output <- pure (lookup "output" kvPairs)
  pure (part, name, input, output)

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
