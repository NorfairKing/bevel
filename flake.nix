{
  description = "bevel";
  nixConfig = {
    extra-substituters = "https://bevel.cachix.org";
    extra-trusted-public-keys = "bevel.cachix.org-1:LaYFysrJKkFZDRCWRsa95GC21eijfHh+IevNeZTqL00=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec/development";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
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
    , weeder-nix
    , validity
    , safe-coloured-text
    , sydtest
    , opt-env-conf
    , autodocodec
    , fast-myers-diff
    , appendful
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
          (import (appendful + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgsMusl = pkgs.pkgsMusl;
      mkNixosModule = import ./nix/nixos-module.nix {
        inherit (pkgsMusl.bevelReleasePackages) bevel-api-server;
        inherit (pkgsMusl.haskellPackages) opt-env-conf;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.bevelRelease;
        static = pkgsMusl.bevelRelease;
      };
      checks.${system} = {
        release = self.packages.${system}.default;
        static = self.packages.${system}.static;
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
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = builtins.attrNames pkgs.haskellPackages.bevelPackages;
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            deadnix.enable = true;
            deadnix.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "bevel-shell";
        packages = p: builtins.attrValues p.bevelPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          cabal-install
          cargo
          clippy
          pkg-config
          rust-analyzer
          rustc
          rustfmt
          zlib
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix {
        inherit (pkgsMusl.bevelReleasePackages)
          bevel-cli
          bevel-gather
          bevel-harness
          bevel-select;
        inherit (pkgsMusl.haskellPackages) opt-env-conf;
      };
    };
}
