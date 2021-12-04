{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hsFinal: hsPrev: {
            advent-of-code = hsFinal.callCabal2nix "advent-of-code" ./. { };
          };
        };
      };
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          hs = pkgs.haskellPackages;
        in
        rec {
          packages = rec {
            advent-of-code = hs.advent-of-code;
            default = advent-of-code;
          };

          apps = rec {
            advent-of-code = flake-utils.lib.mkApp { drv = packages.advent-of-code; };
            default = advent-of-code;
          };

          devShells.default = hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ advent-of-code ];
            nativeBuildInputs = with hs; [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
            ];
          };
        }) // {
      overlays.default = overlay;
    };
}
