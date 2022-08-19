{
  description = "Advent of Code Framework for Haskell";

  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hs = pkgs.haskell.packages.ghc924;
      aoc = hs.callCabal2nix "advent-of-code" ./. { };
    in {
      devShells.${system} = {
        default = hs.shellFor {
          packages = _: [ aoc ];
          nativeBuildInputs = with hs; [
            cabal-install
            fourmolu
            haskell-language-server
            hlint
          ];
        };
      };
    };
}
