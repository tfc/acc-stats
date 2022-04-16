{
  description = "ACC Stats Service";

  inputs = {
    haskellNix.url = github:input-output-hk/haskell.nix;
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      projectOverlay = final: prev: {
        # This overlay adds our project to pkgs
        accStats =
          final.haskell-nix.project' {
            src = final.lib.sourceByRegex ./. [
              "^acc-.*"
              "cabal.project"
            ];
            compiler-nix-name = "ghc8107";
            # This is used by `nix develop .` to open a shell for use with
            # `cabal`, `hlint` and `haskell-language-server`
            shell.tools = {
              cabal = {};
              hlint = {};
              haskell-language-server = {};
            };
            # Non-Haskell shell tools go here
            shell.buildInputs = with pkgs; [
              nixpkgs-fmt
            ];
            # This adds `js-unknown-ghcjs-cabal` to the shell.
            shell.crossPlatforms = p: [p.ghcjs];
          };
      };
      overlays = [ haskellNix.overlay projectOverlay ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.accStats.flake {
        crossPlatforms = p: [p.ghcjs];
      };
      windowsPkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
        crossSystem = pkgs.lib.systems.examples.mingwW64;
      };
      windowsFlake = windowsPkgs.accStats.flake { };
    in pkgs.lib.recursiveUpdate flake {
      defaultPackage = flake.packages."acc-stats-server:exe:acc-stats-server";
      packages.acc-stats-client-windows =
        windowsFlake.packages."acc-stats-client:exe:acc-stats-client";
    });
}
