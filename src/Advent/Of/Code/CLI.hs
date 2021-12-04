module Advent.Of.Code.CLI
  ( Action (..),
    InputSource (..),
    Opts (..),
    execParser,
    parser,
  )
where

import Advent (Day, Part (..), mkDay)
import Control.Monad ((<=<))
import Data.Foldable (asum)
import Options.Applicative
import Text.Read (readMaybe)

data Opts = MkOpts
  { day :: Maybe Day,
    part :: Maybe Part,
    action :: Action,
    inputSource :: InputSource
  }

data InputSource = API | Stdin | InputFile FilePath

data Action
  = Submit
  | ShowInput
  | ShowOutput
  | ShowPrompt
  | Test FilePath
  | Benchmark

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
