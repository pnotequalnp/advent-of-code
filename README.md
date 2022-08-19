# Advent of Code
Advent of Code framework for Haskell based on
[`advent-of-code-api`](https://github.com/mstksg/advent-of-code-api) by Justin Le. Includes fetching
inputs, running solutions, testing examples, submitting solutions, and benchmarking solutions all
out of the box.

## Usage
```haskell
-- Main.hs
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent.Of.Code
import Day1

adventOfCode "Your Name" 2021
```
```haskell
-- Day1.hs
module Day1 where

import Control.Comonad (extend)
import Data.List.NonEmpty (NonEmpty (..))

part1 :: NonEmpty Int -> Int
part1 = sum . extend \case
  x :| y : _ | y > x -> 1
  _ -> 0
```

The name you give is used for the program description with `-h`.

## Running
To see your output for day 3 part 2, run `yourExecutable 3 2`. Run it with no arguments or `-h` for
more information. Requires your AoC session key to be in the environment variable `AOC_SESSION_KEY`
for interacting with the official API.  The session key can be read from the AoC site's cookies. For
more information:
[Chrome](https://developer.chrome.com/docs/devtools/storage/cookies/),
[Firefox](https://developer.mozilla.org/en-US/docs/Tools/Storage_Inspector/Cookies).

## Adding as a Dependency
### Cabal
Create a `cabal.project` if you don't have one already, and add this repo as a remote repository.
For example:
```
packages: .

source-repository-package
    type: git
    location: https://github.com/pnotequalnp/advent-of-code.git
    tag: <commit_hash>
```
Make sure to specify the hash of the commit you want to use. Then add `advent-of-code` to your
`build-depends` in your `.cabal` file.
