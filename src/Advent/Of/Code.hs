module Advent.Of.Code
  ( Part (..),
    runAdvent,
  )
where

import Advent
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Map ((!))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as O
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Read (readMaybe)

data Opts = MkOpts
  { day :: Day,
    part :: Part,
    action :: Action
  }

data Action
  = Submit
  | ShowInput
  | ShowOutput
  | ShowPrompt

runAdvent :: Integer -> (Int -> Part -> Text -> Maybe Text) -> IO ()
runAdvent year solutions = do
  token <-
    lookupEnv "AOC_SESSION_KEY" >>= \case
      Nothing -> errorAndDie "Session key must be provided in `AOC_SESSION_KEY` environment variable"
      Just token -> pure token

  MkOpts {day, part, action} <- O.execParser parser

  let opts = defaultAoCOpts year token

  case action of
    Submit -> do
      input <- runAoC_ opts (AoCInput day)
      (_, result) <- case solutions (fromInteger $ dayInt day) part input of
        Nothing -> errorAndDie "Solution not implemented"
        Just solution -> runAoC_ opts . AoCSubmit day part . T.unpack $ solution
      putStrLn $ showSubmitRes result
    ShowInput -> runAoC_ opts (AoCInput day) >>= T.putStrLn
    ShowOutput -> do
      input <- runAoC_ opts (AoCInput day)
      case solutions (fromInteger $ dayInt day) part input of
        Nothing -> errorAndDie "Solution not implemented"
        Just solution -> T.putStrLn solution
    ShowPrompt -> do
      prompts <- runAoC_ opts (AoCPrompt day)
      T.putStrLn $ prompts ! part

parser :: ParserInfo Opts
parser = O.info (O.helper <*> parseOpts) O.fullDesc

parseOpts :: Parser Opts
parseOpts = MkOpts <$> parseDay <*> parsePart <*> parseAction

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
  O.flag' Submit (O.long "submit" <> O.short 's')
    <|> O.flag' ShowInput (O.long "input" <> O.short 'i')
    <|> O.flag' ShowOutput (O.long "output" <> O.short 'o')
    <|> O.flag' ShowPrompt (O.long "prompt" <> O.short 'p')
    <|> pure ShowOutput

errorAndDie :: Text -> IO a
errorAndDie msg = T.hPutStrLn stderr msg *> exitFailure
