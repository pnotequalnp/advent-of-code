module Advent.Of.Code.Options where

import Advent (Day, Part (..), pattern DayInt)
import Options.Applicative
import Options.Applicative.Types (readerAsk)

data Options = Options
  { dayChoice :: Maybe Day
  , partChoice :: Maybe Part
  , mode :: Mode
  }

data Mode
  = Prompt
  | Input
  | Check
  | Run {submit :: Bool}
  | Test
  | Benchmark

parser :: String -> Integer -> ParserInfo Options
parser name year =
  info
    (parseOpts <**> helper)
    ( fullDesc
        <> header ("Advent of Code " <> show year <> " solutions by " <> name)
        <> footer "Built with https://github.com/pnotequalnp/advent-of-code"
    )

parseOpts :: Parser Options
parseOpts = Options <$> optional day <*> optional part <*> mode

day :: Parser Day
day = argument readDay $ metavar "DAY"
  where
    readDay = do
      x <-
        auto <|> do
          input <- readerAsk
          fail ("Invalid day `" <> input <> "`")
      case x of
        DayInt d -> pure d
        _ -> fail "Day is out of range (1-25 inclusive)"

part :: Parser Part
part = argument readPart $ metavar "PART"
  where
    readPart = do
      x <-
        auto @Int <|> do
          input <- readerAsk
          fail ("Invalid part `" <> input <> "`")
      case x of
        1 -> pure Part1
        2 -> pure Part2
        _ -> fail "Part is out of range (1-2 inclusive)"

mode :: Parser Mode
mode =
  asum
    [ flag' Prompt (long "prompt" <> short 'p' <> help "print the prompt as HTML")
    , flag' Input (long "input" <> short 'i' <> help "print your official input")
    , flag' Check (long "check" <> short 'c' <> help "run the solution locally against input from stdin")
    , flag' (Run False) (long "run" <> short 'r' <> help "run the solution against your official input and print it (default)")
    , flag' (Run True) (long "submit" <> short 's' <> help "run the solution against your official input and submit it")
    , flag' Test (long "test" <> short 't' <> help "test the solution(s) locally")
    , flag' Benchmark (long "bench" <> short 'b' <> help "test and benchmark the solution(s) locally")
    , pure (Run False)
    ]
