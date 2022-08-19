module Advent.Of.Code.Diagnostics (Diagnostics, getDiagnostics, makeBenchmarks, makeTests) where

import Advent (Day, Part (..), dayInt, partInt)
import Advent.Of.Code.Solution (Solution)
import Advent.Of.Code.Toml (toml)
import Control.Applicative (Alternative (..), optional)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench (bench, bgroup, nfAppIO)
import Test.Tasty.HUnit (testCase, (@=?))
import Text.Megaparsec (parseMaybe)

type Diagnostics = [(Int, [(Int, [(String, Text, String)])])]

getDiagnostics :: FilePath -> IO Diagnostics
getDiagnostics fp = successes [minBound .. maxBound] \day -> do
  file <- readFile (fp <> "/day" <> show (dayInt day) <> ".toml")
  Just diags <- pure (parseMaybe toml file)
  parts@(_ : _) <- successes [minBound .. maxBound] \part -> do
    Just partDiags@(_ : _) <- pure (lookup part diags)
    pure (fromEnum part, partDiags)
  pure (fromEnum day, parts)

makeTests :: Diagnostics -> (Day -> Part -> Maybe Solution) -> TestTree
makeTests diags solutions = testGroup "advent of code" do
  (toEnum -> day, dayDiags) <- diags
  dayTests@(_ : _) <- pure do
    (toEnum -> part, partDiags) <- dayDiags
    solution <- maybeToList (solutions day part)
    pure $ testGroup ("part " <> show (partInt part)) do
      (name, input, expected) <- partDiags
      pure $ testCase name do
        actual <- solution input
        expected @=? actual
  pure $ testGroup ("day " <> show (dayInt day)) dayTests

makeBenchmarks :: Diagnostics -> (Day -> Part -> Maybe Solution) -> TestTree
makeBenchmarks diags solutions = bgroup "advent of code" do
  (toEnum -> day, dayDiags) <- diags
  dayBenches@(_ : _) <- pure do
    (toEnum -> part, partDiags) <- dayDiags
    solution <- maybeToList (solutions day part)
    pure $ testGroup ("part " <> show (partInt part)) do
      (name, !input, _) <- partDiags
      pure $ bench name (nfAppIO solution input)
  pure $ testGroup ("day " <> show (dayInt day)) dayBenches

successes :: Alternative f => [a] -> (a -> f b) -> f [b]
successes xs f = catMaybes <$> traverse (optional . f) xs
