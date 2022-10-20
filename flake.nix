{
  description = "bevel";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity = {
      url = "github:NorfairKing/validity?ref=flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        autodocodec.follows = "autodocodec";
        sydtest.follows = "sydtest";
        safe-coloured-text.follows = "safe-coloured-text";
      };
    };
    autodocodec = {
      url = "github:NorfairKing/autodocodec?ref=flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        validity.follows = "validity";
        sydtest.follows = "sydtest";
        safe-coloured-text.follows = "safe-coloured-text";
      };
    };
    safe-coloured-text = {
      url = "github:NorfairKing/safe-coloured-text?ref=flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        validity.follows = "validity";
        autodocodec.follows = "autodocodec";
        sydtest.follows = "sydtest";
      };
    };
    sydtest = {
      url = "github:NorfairKing/sydtest?ref=flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        validity.follows = "validity";
        autodocodec.follows = "autodocodec";
        safe-coloured-text.follows = "safe-coloured-text";
      };
    };
    appendful = {
      url = "github:NorfairKing/appendful?ref=flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        validity.follows = "validity";
        autodocodec.follows = "autodocodec";
        sydtest.follows = "sydtest";
        safe-coloured-text.follows = "safe-coloured-text";
      };
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , flake-utils
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , appendful
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            self.overlays.${system}
            autodocodec.overlays.${system}
            safe-coloured-text.overlays.${system}
            sydtest.overlays.${system}
            validity.overlays.${system}
            appendful.overlays.${system}
          ];
        };
        pkgs = pkgsFor nixpkgs;

      in
      {
        overlays = final: prev:
          with final.lib;
          with final.haskell.lib;
          {

            bevel-gather = final.callPackage ./bevel-gather/default.nix { };
            bevel-harness = final.callPackage ./bevel-harness/default.nix { };

            bevelReleasePackages = mapAttrs (_: pkg: justStaticExecutables (doCheck pkg)) final.haskellPackages.bevelPackages // {
              inherit (final)
                bevel-gather
                bevel-harness;
            };
            bevelRelease =
              final.symlinkJoin {
                name = "bevel-release";
                paths = builtins.attrValues final.bevelReleasePackages;
              };


            haskellPackages = prev.haskellPackages.override (old: {
              overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
                (
                  self: super:
                    let
                      bevelPkg = name:
                        buildFromSdist (overrideCabal (self.callPackage (./${name}/default.nix) { }) (old: {
                          configureFlags = (old.configureFlags or [ ]) ++ [
                            # Optimisations
                            "--ghc-options=-O2"
                            # Extra warnings
                            "--ghc-options=-Wall"
                            "--ghc-options=-Wincomplete-uni-patterns"
                            "--ghc-options=-Wincomplete-record-updates"
                            "--ghc-options=-Wpartial-fields"
                            "--ghc-options=-Widentities"
                            "--ghc-options=-Wredundant-constraints"
                            "--ghc-options=-Wcpp-undef"
                            "--ghc-options=-Werror"
                          ];
                          doBenchmark = true;
                          doHaddock = false;
                          doCoverage = false;
                          doHoogle = false;
                          doCheck = false; # Only check the release version.
                          hyperlinkSource = false;
                          enableLibraryProfiling = false;
                          enableExecutableProfiling = false;
                          buildDepends = (old.buildDepends or [ ]) ++ (with final; [
                            haskellPackages.autoexporter
                          ]);
                          # Ugly hack because we can't just add flags to the 'test' invocation.
                          # Show test output as we go, instead of all at once afterwards.
                          testTarget = (old.testTarget or "") + " --show-details=direct";
                        }));
                      bevelPkgWithComp =
                        exeName: name:
                        generateOptparseApplicativeCompletion exeName (bevelPkg name);
                      bevelPkgWithOwnComp = name: bevelPkgWithComp name name;

                      bevelPackages = {
                        "bevel-api" = bevelPkg "bevel-api";
                        "bevel-api-gen" = bevelPkg "bevel-api-gen";
                        "bevel-api-server" = bevelPkgWithOwnComp "bevel-api-server";
                        "bevel-api-server-gen" = bevelPkg "bevel-api-server-gen";
                        "bevel-api-server-data" = bevelPkg "bevel-api-server-data";
                        "bevel-api-server-data-gen" = bevelPkg "bevel-api-server-data-gen";
                        "bevel-cli" = addTestToolDepend (bevelPkgWithComp "bevel" "bevel-cli") final.bevel-gather;
                        "bevel-client" = bevelPkg "bevel-client";
                        "bevel-client-data" = bevelPkg "bevel-client-data";
                        "bevel-client-data-gen" = bevelPkg "bevel-client-data-gen";
                        "bevel-data" = bevelPkg "bevel-data";
                        "bevel-data-gen" = bevelPkg "bevel-data-gen";
                      };
                    in
                    {
                      inherit bevelPackages;

                      bevelRelease =
                        final.symlinkJoin {
                          name = "bevel-release";
                          paths = final.lib.attrValues self.bevelPackages;
                        };
                    } // bevelPackages
                );
            });
          };

        packages.release = pkgs.bevelRelease;
        packages.default = self.packages.${system}.release;
        checks = {
          nixos-module-test = import ./nix/nixos-module-test.nix {
            inherit pkgs;
            home-manager = home-manager.nixosModules.home-manager;
            bevel-nixos-module-factory = self.nixosModuleFactories.${system}.default;
            bevel-home-manager-module = self.homeManagerModules.${system}.default;
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
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "bevel-shell";
          packages = (p:
            (builtins.attrValues p.bevelPackages)
          );
          withHoogle = true;
          doBenchmark = true;
          buildInputs = with pkgs; [
            niv
            zlib
            cabal-install
          ] ++ (with pre-commit-hooks;
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
        nixosModuleFactories.default = import ./nix/nixos-module.nix;
        homeManagerModules.default = import ./nix/home-manager-module.nix { bevelReleasePackages = pkgs.bevelReleasePackages; };
      });
}
