module Advent.Of.Code
  ( Part (..),
    runAdvent,
  )
where

import Advent
import Control.Applicative (optional)
import Control.Monad ((<=<))
import Criterion.Main qualified as C
import Data.Foldable (asum)
import Data.Map ((!))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as O
import System.Environment (getArgs, getProgName, lookupEnv, withArgs, withProgName)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Read (readMaybe)

data Opts = MkOpts
  { day :: Day,
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
  | Benchmark

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

  MkOpts {day, part, action, inputSource} <- withArgs args $ O.execParser parser

  let opts = defaultAoCOpts year token
      fetchPart = maybe (errorAndDie "Part required") pure part
      fetchSolution =
        fetchPart >>= maybe (errorAndDie "Solution not implemented") pure . solutions day
      fetchInput = case inputSource of
        API -> runAoC_ opts (AoCInput day)
        Stdin -> T.getContents
        InputFile filepath -> T.readFile filepath
      runSolution = fetchSolution <*> fetchInput

  case action of
    Submit -> do
      case inputSource of
        API -> pure ()
        _ -> T.hPutStrLn stderr "WARNING: using non-API input to submit"
      part' <- fetchPart
      solution <- runSolution
      (_, result) <- runAoC_ opts . AoCSubmit day part' . T.unpack $ solution
      putStrLn $ showSubmitRes result
    ShowInput -> fetchInput >>= T.putStrLn
    ShowOutput -> runSolution >>= print
    ShowPrompt -> do
      part' <- fetchPart
      prompt <- runAoC_ opts (AoCPrompt day)
      T.putStrLn $ prompt ! part'
    Benchmark -> do
      solution <- fetchSolution
      progName <- getProgName
      let bench = C.env fetchInput $ C.bench "solution" . C.nf solution
          progName' = unwords (progName : args ++ ["--"])
      withProgName progName' . withArgs extraArgs $ C.defaultMain [bench]

parser :: ParserInfo Opts
parser = O.info (O.helper <*> parseOpts) O.fullDesc

parseOpts :: Parser Opts
parseOpts = MkOpts <$> parseDay <*> optional parsePart <*> parseAction <*> parseInput

parseDay :: Parser Day
parseDay = O.argument readDay $ O.metavar "DAY"
  where
    readDay = O.maybeReader $ mkDay <=< readMaybe

parsePart :: Parser Part
parsePart = O.argument readPart $ O.metavar "PART"
  where
    readPart = O.maybeReader \case
      "1" -> Just Part1
      "2" -> Just Part2
      _ -> Nothing

parseAction :: Parser Action
parseAction =
  asum
    [ O.flag' Submit (O.long "submit" <> O.short 's'),
      O.flag' ShowInput (O.long "input" <> O.short 'i'),
      O.flag' ShowOutput (O.long "output" <> O.short 'o'),
      O.flag' ShowPrompt (O.long "prompt" <> O.short 'p'),
      O.flag' Benchmark (O.long "bench" <> O.short 'b'),
      pure ShowOutput
    ]

parseInput :: Parser InputSource
parseInput =
  asum
    [ O.flag' API (O.long "api"),
      O.flag' Stdin (O.long "stdin"),
      InputFile <$> O.strOption (O.long "file" <> O.short 'f' <> O.metavar "PATH"),
      pure API
    ]

errorAndDie :: Text -> IO a
errorAndDie msg = T.hPutStrLn stderr msg *> exitFailure
