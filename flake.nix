{
  description = "ACC Stats Service";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        filteredSrc = pkgs: pkgs.lib.sourceByRegex ./. [
          "^acc-.*"
          "cabal.project"
        ];
        projectOverlay = final: prev: {
          acc-stats =
            final.haskell-nix.project' {
              src = filteredSrc final;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                cabal-install
                haskellPackages.cabal-fmt
                haskellPackages.ghcid
                hlint
                inotify-tools
                nixpkgs-fmt
                statix
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

        packages = {
          default = flake.packages."acc-stats-server:exe:acc-stats-server";
          acc-stats-client-windows =
            windowsFlake.packages."acc-stats-client:exe:acc-stats-client";
        };

        checks = {
          cabal-fmt-check = pkgs.runCommand "cabal-fmt-check" {} ''
            ${pkgs.haskellPackages.cabal-fmt}/bin/cabal-fmt \
              -c ${filteredSrc pkgs}/**/*.cabal \
              |& tee $out
          '';

          hlint-check = pkgs.runCommand "hlint-check" {} ''
            ${pkgs.haskellPackages.hlint}/bin/hlint ${filteredSrc pkgs} \
              |& tee $out
          '';

          statix-check = pkgs.runCommand "statix-check" {} ''
            ${pkgs.statix}/bin/statix check ${./.} \
              |& tee $out
          '';
        };
      });
}
