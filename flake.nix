{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskellPackages;
      in rec {
        packages.advent-of-code = hs.callCabal2nix "advent-of-code" ./. { };
        defaultPackage = packages.advent-of-code;

        devShell = packages.advent-of-code.env.overrideAttrs
          (super: { buildInputs = super.buildInputs ++ [ hs.cabal-install ]; });
      });
}
