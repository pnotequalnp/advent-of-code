# Advent of Code
Advent of Code framework for Haskell based on
[`advent-of-code-api`](https://github.com/mstksg/advent-of-code-api) by Justin Le. Includes fetching
inputs, running solutions, testing examples, submitting solutions, and benchmarking solutions all
out of the box.

## Usage
```haskell
import Advent.Of.Code (Day (..), Part (..), runAdvent)
import Data.Text (Text)

main :: IO ()
main = runAdvent 2021 solve
  where
    solve Day1 Part1 = Just day1part1
    solve Day7 Part2 = Just day7part2
    solve _ _ = Nothing
    
day1part1 :: Text -> Text
day1part1 input = "This is probably not the correct answer"
    
day7part2 :: Text -> Text
day7part2 input = "This is also probably wrong"
```
I recommend putting each day in its own module. Check out 
[`Advent.Of.Code.Input`](https://pnotequalnp.github.io/advent-of-code/Advent-Of-Code-Input.html)
for convenience functions for wrangling the inputs into a more manageable form.

## Running
To see your output for day 3 part 2, run `yourExecutable 3 2`. Run it with no arguments or `-h` for
more information. Requires your AoC session key to be in the environment variable `AOC_SESSION_KEY`.
The session key can be read from the AoC site's cookies. For more information:
[Chrome](https://developer.chrome.com/docs/devtools/storage/cookies/),
[Firefox](https://developer.mozilla.org/en-US/docs/Tools/Storage_Inspector/Cookies).

## Adding as a Dependency
### Nix (recommended)
Add this flake as an input, and apply its default overlay when importing Nixpkgs. Add
`advent-of-code` to your `build-depends` in your Cabal file.

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
Make sure to specify the hash of the commit you want to use. Add `advent-of-code` to your
`build-depends` in your Cabal file.
