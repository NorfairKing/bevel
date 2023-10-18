{
  description = "bevel";
  nixConfig = {
    extra-substituters = "https://bevel.cachix.org";
    extra-trusted-public-keys = "bevel.cachix.org-1:LaYFysrJKkFZDRCWRsa95GC21eijfHh+IevNeZTqL00=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-23.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    appendful.url = "github:NorfairKing/appendful";
    appendful.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , fast-myers-diff
    , appendful
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (appendful + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixosModule = import ./nix/nixos-module.nix { inherit (pkgs.bevelReleasePackages) bevel-api-server; };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.bevelRelease;
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit (pkgs) nixosTest;
          home-manager = home-manager.nixosModules.home-manager;
          bevel-nixos-module-factory = self.nixosModuleFactories.${system}.default;
          bevel-home-manager-module = self.homeManagerModules.${system}.default;
        };
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "bevel-api"
            "bevel-api-server"
            "bevel-client"
            "bevel-client-data"
            "bevel-data"
            "bevel-api-gen"
            "bevel-api-server-data"
            "bevel-cli"
            "bevel-api-server-data-gen"
            "bevel-api-server-gen"
            "bevel-client-data-gen"
            "bevel-data-gen"
          ];
          # Not haskell packages:
          # "bevel-gather"
          # "bevel-harness"
          # "bevel-select"
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "bevel-shell";
        packages = p: builtins.attrValues p.bevelPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          cabal-install
          cargo
          clippy
          pkg-config
          rust-analyzer
          rustc
          rustfmt
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix { bevelReleasePackages = pkgs.bevelReleasePackages; };
    };
}
