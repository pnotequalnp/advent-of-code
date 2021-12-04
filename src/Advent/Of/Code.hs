module Advent.Of.Code
  ( Part (..),
    runAdvent,
  )
where

import Advent
import Advent.Of.Code.CLI (Action (..), InputSource (..), Opts (..), execParser, parser)
import Advent.Of.Code.Test (makeTests)
import Advent.Of.Code.Test.Parsing (parseTests, prettyTomlDecodeErrors)
import Criterion.Main qualified as Criterion
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs, getProgName, lookupEnv, withArgs, withProgName)
import System.Exit (exitFailure)
import System.IO (stderr)
import Test.Tasty qualified as Tasty

runAdvent :: Integer -> (Int -> Part -> Maybe (Text -> Text)) -> IO ()
runAdvent year ((. fromInteger . dayInt) -> solutions) = do
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

errorAndDie :: Text -> IO a
errorAndDie msg = T.hPutStrLn stderr msg *> exitFailure
