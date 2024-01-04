{
  description = "Advent of code 2023";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', system, config, pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          # The base package set (this value is the default)
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`
          packages = {
            # Add source or Hackage overrides here
            # (Local packages are added automatically)
            /* aeson.source = "1.5.0.0" # Hackage version
               shower.source = inputs.shower; # Flake input
            */
          };

          # Add your package overrides here
          settings = {
            /* haskell-template = {
                 haddock = false;
               };
               aeson = {
                 check = false;
               };
            */
          };

          # Development shell configuration
          devShell = { hlsCheck.enable = false; };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.nixfmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          settings.formatter.ormolu = {
            options = [ "--ghc-opt" "-XImportQualifiedPost" ];
          };
        };

        mission-control.scripts = {
          test = {
            description = "Run all tests";
            exec = ''
              ghcid -c "cabal repl test:tests" -T :main
            '';
          };
        };

        # Default package & app.
        packages.default = self'.packages.advent23;
        apps.default = self'.apps.advent23;

        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "advent23";
          meta.description = "Haskell development environment";
          # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [ just nixci ];
        };
      };
    };
}
