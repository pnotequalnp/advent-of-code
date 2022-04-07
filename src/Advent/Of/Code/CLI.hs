-- |
-- Module      : Advent.Of.Code.CLI
-- Description : Advent of Code framework CLI parser
-- Copyright   : Kevin Mullins 2021-2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
-- Portability : portable
--
-- = Advent.Of.Code.CLI
-- Basic CLI parser for the framework.
module Advent.Of.Code.CLI
  ( -- * Parsing
    parser,
    execParser,

    -- * Types
    Action (..),
    InputSource (..),
    Opts (..),
  )
where

import Advent (Day, Part (..), mkDay)
import Control.Monad ((<=<))
import Data.Foldable (asum)
import Options.Applicative
import Text.Read (readMaybe)

-- | The configuration for the framework.
data Opts = MkOpts
  { day :: Maybe Day,
    part :: Maybe Part,
    action :: Action,
    inputSource :: InputSource
  }

-- | Where to fetch the input from.
data InputSource
  = -- | Fetch from the AoC API
    API
  | -- | Read from stdin
    Stdin
  | -- | Read from the specified file
    InputFile FilePath

-- | The possible modes of action.
data Action
  = -- | Compute solution and submit via the AoC API
    Submit
  | -- | Show the input without computing the solution
    ShowInput
  | -- | Compute and print the solution without submitting
    ShowOutput
  | -- | Show the prompt without computing the solution
    ShowPrompt
  | -- | Test solutions against the examples provided in a file
    Test FilePath
  | -- | Benchmark solutions against the input
    Benchmark

-- | Advent of Code framework CLI option parser.
parser :: ParserInfo Opts
parser = info (helper <*> parseOpts) fullDesc

parseOpts :: Parser Opts
parseOpts = MkOpts <$> optional parseDay <*> optional parsePart <*> parseAction <*> parseInput

parseDay :: Parser Day
parseDay = argument readDay $ metavar "DAY"
  where
    readDay = maybeReader $ mkDay <=< readMaybe

parsePart :: Parser Part
parsePart = argument readPart $ metavar "PART"
  where
    readPart = maybeReader \case
      "1" -> Just Part1
      "2" -> Just Part2
      _ -> Nothing

parseAction :: Parser Action
parseAction =
  asum
    [ flag' Submit (long "submit" <> short 's'),
      flag' ShowInput (long "input" <> short 'i'),
      flag' ShowOutput (long "output" <> short 'o'),
      flag' ShowPrompt (long "prompt" <> short 'p'),
      Test <$> strOption (long "test" <> short 't' <> metavar "PATH" <> value "tests.toml"),
      flag' Benchmark (long "bench" <> short 'b'),
      pure ShowOutput
    ]

parseInput :: Parser InputSource
parseInput =
  asum
    [ flag' API (long "api"),
      flag' Stdin (long "stdin"),
      InputFile <$> strOption (long "file" <> short 'f' <> metavar "PATH"),
      pure API
    ]
