{-# LANGUAGE TemplateHaskell #-}

module Advent.Of.Code (
  adventOfCode,
  adventOfTests,
  adventOfBenchmarks,
  runAdventOfCode,
  Solution,
  IsSolution (..),
) where

import Advent (AoC (..), AoCError (..), AoCOpts, Day, Part (..), dayInt, defaultAoCOpts, mkDay_, partInt, runAoC_, showSubmitRes)
import Advent.Of.Code.Diagnostics (getDiagnostics, makeBenchmarks, makeTests)
import Advent.Of.Code.Options (Mode (..), Options (..), parser)
import Advent.Of.Code.Solution (IsSolution (..), Solution)
import Control.Exception (Exception (..), Handler (..), SomeAsyncException (..), SomeException (..), catch, catches, evaluate, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Text.IO qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Language.Haskell.TH (Dec, Exp (..), Lit (..), Q, lookupValueName)
import Options.Applicative (execParser)
import System.Environment (getArgs, getProgName, lookupEnv, withArgs, withProgName)
import System.Exit (die)
import System.IO.Error (catchIOError)
import Test.Tasty qualified as Tasty
import Test.Tasty.Bench qualified as Tasty.Bench

data Action
  = ShowPrompt {day :: Day, part :: Part, opts :: AoCOpts}
  | ShowInput {day :: Day, opts :: AoCOpts}
  | RunStdin {solution :: Solution}
  | RunOfficial {day :: Day, part :: Part, solution :: Solution, opts :: AoCOpts, submit :: Bool}

resultBound :: Int
resultBound = 2048

runAdventOfCode :: Integer -> (Day -> Part -> Maybe Solution) -> IO ()
runAdventOfCode year solutions = do
  progName <- getProgName
  (args, extraArgs') <- span (/= "--") <$> getArgs
  let extraArgs = case extraArgs' of
        "--" : xs -> xs
        xs -> xs
      progName' = unwords (progName : args ++ ["--"])

  Options {dayChoice, partChoice, mode} <- withArgs args (execParser parser)

  let getDay = case dayChoice of
        Nothing -> die "Day required"
        Just d -> pure d
      getPart = case partChoice of
        Nothing -> die "Part required"
        Just p -> pure p
      getSolution d p = case solutions d p of
        Nothing ->
          die $ concat ["Solution for day ", show (dayInt d), ", part ", show (partInt p), " not implemented"]
        Just s -> pure s
      getOpts = do
        res <- lookupEnv "AOC_SESSION_KEY"
        case res of
          Nothing -> die "Session key must be provided in `AOC_SESSION_KEY` environment variable"
          Just token -> pure (defaultAoCOpts year token)

  action <- case mode of
    Prompt -> ShowPrompt <$> getDay <*> getPart <*> getOpts
    Input -> ShowInput <$> getDay <*> getOpts
    Check -> do
      day <- getDay
      part <- getPart
      solution <- getSolution day part
      pure RunStdin {solution}
    Run {submit} -> do
      day <- getDay
      part <- getPart
      solution <- getSolution day part
      opts <- getOpts
      pure RunOfficial {day, part, solution, opts, submit}

  withProgName progName' (withArgs extraArgs (runAction action)) `catch` \case
    AoCClientError _ -> die "Fatal: an error occurred in the HTTP client"
    AoCThrottleError -> die "Fatal: throttle limit exhausted"
    AoCReleaseError dt -> die (formatTime defaultTimeLocale formatString dt)
      where
        formatString = "Challenge not yet available (%d days, %H hours, %M minutes, %S seconds remaining)"

runAction :: Action -> IO ()
runAction = \case
  ShowPrompt {day, part, opts} -> do
    prompts <- runAoC_ opts (AoCPrompt day)
    case M.lookup part prompts of
      Nothing -> die "Prompt not unlocked"
      Just prompt -> T.putStrLn prompt
  ShowInput {day, opts} -> do
    input <- runAoC_ opts (AoCInput day)
    T.putStrLn input
  RunStdin {solution} -> do
    input <- T.getContents `catchIOError` \_ -> die "Fatal: failed to read stdin"
    output <- runSolution input solution
    putStrLn output
  RunOfficial {day, part, solution, opts, submit} -> do
    input <- runAoC_ opts (AoCInput day)
    output <- runSolution input solution
    if submit
      then do
        (_, res) <- runAoC_ opts (AoCSubmit day part output)
        putStrLn (showSubmitRes res)
      else putStrLn output
  where
    runSolution input solution = runSolution' input solution `catches` [Handler handleAsync, Handler handleSync]
    runSolution' input solution = do
      result <- solution input
      let (prefix, remainder) = splitAt resultBound result
      unless (null remainder) do
        die ("Fatal: result exceeds " <> show resultBound <> " characters")
      traverse evaluate prefix
    handleAsync e@(SomeAsyncException _) = throwIO e
    handleSync (SomeException e) = die ("Fatal: an exception was thrown during evaluation of the result:\n\n" <> displayException e)

adventOfCode :: Int -> Q [Dec]
adventOfCode year =
  [d|
    main :: IO ()
    main = $run year (curry (`lookup` solutions))
      where
        solutions = $getSolutions
    |]
  where
    run = pure (VarE 'runAdventOfCode)

adventOfTests :: FilePath -> Q [Dec]
adventOfTests fp = do
  diags <- liftIO (getDiagnostics fp)
  [d|
    main :: IO ()
    main = Tasty.defaultMain (makeTests diags (curry (`lookup` solutions)))
      where
        solutions = $getSolutions
    |]

adventOfBenchmarks :: FilePath -> Q [Dec]
adventOfBenchmarks fp = do
  diags <- liftIO (getDiagnostics fp)
  [d|
    main :: IO ()
    main = Tasty.Bench.defaultMain (makeBenchmarks diags (curry (`lookup` solutions)))
      where
        solutions = $getSolutions
    |]

getSolutions :: Q Exp
getSolutions = do
  xs <- sequence $ lookupSolution <$> [minBound .. maxBound] <*> [minBound .. maxBound]
  pure $ ListE (concat xs)
  where
    lookupSolution d p =
      lookupValueName (toName d p) >>= \case
        Nothing -> pure []
        Just n -> pure <$> [|($(toKey d p), $(pure solve') $(pure (VarE n)))|]
    solve' = VarE 'solve
    toName d p = concat ["Day", show (dayInt d), ".part", show (partInt p)]
    toKey d p =
      let d' = dayInt d
          p' = case p of
            Part1 -> 'Part1
            Part2 -> 'Part2
       in pure $ TupE [Just (AppE (VarE 'mkDay_) (LitE (IntegerL d'))), Just (ConE p')]
