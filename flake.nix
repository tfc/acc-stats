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
          acc-stats =
            final.haskell-nix.project' {
              src = final.lib.sourceByRegex ./. [
                "^acc-.*"
                "cabal.project"
              ];
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        };
        overlays = [ haskellNix.overlay projectOverlay ];
        nixpkgsConfig = {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        pkgs = import nixpkgs nixpkgsConfig;
        flake = pkgs.acc-stats.flake { };
        windowsPkgs = import nixpkgs (nixpkgsConfig // {
          crossSystem = pkgs.lib.systems.examples.mingwW64;
        });
        windowsFlake = windowsPkgs.acc-stats.flake { };
      in
      pkgs.lib.recursiveUpdate flake {
        defaultPackage = flake.packages."acc-stats-server:exe:acc-stats-server";
        packages.acc-stats-client-windows =
          windowsFlake.packages."acc-stats-client:exe:acc-stats-client";
      });
}
