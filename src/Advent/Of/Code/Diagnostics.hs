module Advent.Of.Code.Diagnostics where

import Advent (Day, Part (..), dayInt, partInt)
import Advent.Of.Code.Solution (Solution)
import Advent.Of.Code.Toml (toml)
import Control.Applicative (Alternative (..), optional)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import System.Exit (die)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nfAppIO)
import Test.Tasty.HUnit (testCase, (@=?))
import Text.Megaparsec (parseMaybe)

data Diagnostic = Tests | Benchmarks

makeDiagnostics :: Diagnostic -> Maybe Day -> Maybe Part -> (Day -> Part -> Maybe Solution) -> IO TestTree
makeDiagnostics diag day part solutions =
  testGroup "advent of code" <$> case day of
    Nothing -> successes [minBound .. maxBound] \d -> makeDiagDay diag d part solutions
    Just d -> do
      tests <- optional (makeDiagDay diag d part solutions)
      case tests of
        Nothing -> die "Day has no tests and/or solutions"
        Just ts -> pure [ts]

makeDiagDay :: Diagnostic -> Day -> Maybe Part -> (Day -> Part -> Maybe Solution) -> IO TestTree
makeDiagDay diag day part solutions = do
  file <- readFile ("tests/day" <> show (dayInt day) <> ".toml")
  Just pairs <- pure (parseMaybe toml file)
  testGroup ("day " <> show (dayInt day)) <$> case part of
    Nothing -> successes (zip [minBound .. maxBound] pairs) \(p, partTests) ->
      makeDiagPart diag p (solutions day p) partTests
    Just p -> do
      tests <- optional (makeDiagPart diag p (solutions day p) (pairs !! fromEnum p))
      case tests of
        Nothing -> die "Part has no tests and/or solutions"
        Just ts -> pure [ts]

makeDiagPart :: Diagnostic -> Part -> Maybe Solution -> [(String, String, String)] -> IO TestTree
makeDiagPart diag part solution pairs = do
  s <- case solution of
    Nothing -> empty
    Just s -> pure s
  pure case diag of
    Tests -> testGroup name (makeTestCase s <$> pairs)
    Benchmarks -> bgroup name (makeBenchCase s <$> pairs)
  where
    name = "part " <> show (partInt part)

makeTestCase :: Solution -> (String, String, String) -> TestTree
makeTestCase solution (name, input, expected) =
  testCase name do
    actual <- solution (T.pack input)
    expected @=? actual

makeBenchCase :: Solution -> (String, String, String) -> Benchmark
makeBenchCase solution (name, input, _) = bench name (nfAppIO solution input')
  where
    !input' = T.pack input

successes :: Alternative f => [a] -> (a -> f b) -> f [b]
successes xs f = catMaybes <$> traverse (optional . f) xs
