-- |
-- Module      : Advent.Of.Code
-- Description : Advent of Code framework
-- Copyright   : Kevin Mullins 2021-2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
-- Stability   : unstable
-- Portability : portable
--
-- = Advent.Of.Code
-- A framework for fetching inputs, submitting solutions, testing examples, and benchmarking
-- solutions for [Advent of Code](https://adventofcode.com/).
module Advent.Of.Code
  ( -- * Advent of Code
    runAdvent,
    runAdvent',
    Day (..),
    Part (..),
  )
where

import Advent hiding (Day)
import Advent qualified (Day)
import Advent.Of.Code.CLI (Action (..), InputSource (..), Opts (..), execParser, parser)
import Advent.Of.Code.Test (makeTests)
import Advent.Of.Code.Test.Parsing (parseTests, prettyTomlDecodeErrors)
import Criterion.Main qualified as Criterion
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Version (Version, showVersion)
import Paths_advent_of_code (version)
import System.Environment (getArgs, getProgName, lookupEnv, withArgs, withProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Tasty qualified as Tasty

-- | The day of a challenge.
data Day
  = Day1
  | Day2
  | Day3
  | Day4
  | Day5
  | Day6
  | Day7
  | Day8
  | Day9
  | Day10
  | Day11
  | Day12
  | Day13
  | Day14
  | Day15
  | Day16
  | Day17
  | Day18
  | Day19
  | Day20
  | Day21
  | Day22
  | Day23
  | Day24
  | Day25
  deriving (Enum)

toDay :: Advent.Day -> Day
toDay = toEnum . subtract 1 . fromInteger . dayInt

-- | The primary entry point for the framework. The solutions are expected as a function that takes
-- the day and part and returns a @Maybe (Text -> Text)@. That @Text -> Text@ is used to compute the
-- solution from the input, if it exists.
runAdvent ::
  -- | The AoC year
  Integer ->
  -- | Solutions
  (Day -> Part -> Maybe (Text -> Text)) ->
  IO ()
runAdvent = runAdvent' Nothing

-- | Entry point that specifies the version of your solutions with the @--version@ option. See
-- `runAdvent`.
runAdvent' ::
  -- | Version of the solutions
  Maybe Version ->
  -- | The AoC year
  Integer ->
  -- | Solutions
  (Day -> Part -> Maybe (Text -> Text)) ->
  IO ()
runAdvent' v year ((. toDay) -> solutions) = do
  (args, extraArgs') <- span (/= "--") <$> getArgs
  let extraArgs = case extraArgs' of
        "--" : xs -> xs
        xs -> xs

  token <-
    lookupEnv "AOC_SESSION_KEY" >>= \case
      Nothing -> errorAndDie "Session key must be provided in `AOC_SESSION_KEY` environment variable"
      Just token -> pure token

  MkOpts {day, part, action, inputSource} <- withArgs args $ execParser parser

  let opts = defaultAoCOpts year token
      fetchDay = maybe (errorAndDie "Day required") pure day
      fetchPart = maybe (errorAndDie "Part required") pure part
      fetchSolution = do
        day' <- fetchDay
        part' <- fetchPart
        maybe (errorAndDie "Solution not implemented") pure $ solutions day' part'
      fetchInput = do
        day' <- fetchDay
        case inputSource of
          API -> runAoC_ opts (AoCInput day')
          Stdin -> T.getContents
          InputFile filepath -> T.readFile filepath
      runSolution = fetchSolution <*> fetchInput
      withExtraArgs x = do
        progName <- getProgName
        let progName' = unwords (progName : args ++ ["--"])
        withProgName progName' . withArgs extraArgs $ x

  case action of
    Submit -> do
      case inputSource of
        API -> pure ()
        _ -> T.hPutStrLn stderr "WARNING: using non-API input to submit"
      day' <- fetchDay
      part' <- fetchPart
      solution <- runSolution
      (_, result) <- runAoC_ opts . AoCSubmit day' part' . T.unpack $ solution
      putStrLn $ showSubmitRes result
    ShowInput -> fetchInput >>= T.putStrLn
    ShowOutput -> runSolution >>= print
    ShowPrompt -> do
      day' <- fetchDay
      part' <- fetchPart
      prompt <- runAoC_ opts (AoCPrompt day')
      T.putStrLn $ prompt ! part'
    Test filepath -> do
      testFile <- T.readFile filepath
      case parseTests testFile of
        Left errs -> errorAndDie $ "Couldn't parse test file:\n" <> prettyTomlDecodeErrors errs
        Right tests -> withExtraArgs (Tasty.defaultMain (makeTests solutions tests'))
          where
            tests' = case day of
              Nothing -> tests
              Just d -> case part of
                Nothing -> Map.filterWithKey (\(d', _) _ -> d' == d) tests
                Just p -> Map.filterWithKey (\(d', p') _ -> d' == d && p' == p) tests
    Benchmark -> do
      solution <- fetchSolution
      let bench = Criterion.env fetchInput $ Criterion.bench "solution" . Criterion.nf solution
      withExtraArgs (Criterion.defaultMain [bench])
    Version -> hPutStrLn stderr case v of
      Nothing -> frameworkVersion
      Just v' -> showVersion v' <> " (" <> frameworkVersion <> ")"
      where
        frameworkVersion = "Advent of Code Framework " <> showVersion version

errorAndDie :: Text -> IO a
errorAndDie msg = T.hPutStrLn stderr msg *> exitFailure
