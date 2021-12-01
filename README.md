# Advent of Code
Advent of Code framework for Haskell based on
[`advent-of-code-api`](https://github.com/mstksg/advent-of-code-api) by Justin Le.

## Usage
```haskell
import Advent.Of.Code
import Data.Text (Text)

main :: IO ()
main = runAdvent 2021 solve
  where
    solve 1 Part1 = Just . day1part1
    solve 7 Part2 = Just . day7part2
    solve _ _ = const Nothing
    
day1part1 :: Text -> Text
day1part1 input = "This is probably not the correct answer"
    
day7part2 :: Text -> Text
day7part2 input = "This is also probably wrong"
```
I recommend putting each day in its own module.

## Running
To see your output for day 3 part 2, run `yourExecutable 3 2`. Run it with no arguments or `-h` for
more information. Requires your AoC session key to be in the environment variable `AOC_SESSION_KEY`.
The session key can be read from the AoC site's cookies. For more information:
[Chrome](https://developer.chrome.com/docs/devtools/storage/cookies/),
[Firefox](https://developer.mozilla.org/en-US/docs/Tools/Storage_Inspector/Cookies).
