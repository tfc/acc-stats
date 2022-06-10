{
  description = "ACC Stats Service";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs =
    { self
    , flake-utils
    , haskellNix
    , nixpkgs
    , pre-commit-hooks
    }:
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
            compiler-nix-name = "ghc902";
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
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            statix.enable = true;
            hlint.enable = true;
            cabal-fmt.enable = true;
          };
        };
      };
    });
}
