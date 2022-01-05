final: previous:
with final.lib;
with final.haskell.lib;

{
  bevel-gather = final.callPackage ../bevel-gather/default.nix { };
  bevelHaskellPackages =
    let
      bevelPkg = name:
        overrideCabal
          (
            final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}"))
              "--no-hpack"
              { }
          )
          (old: {
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
          });
      bevelPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (bevelPkg name);
      bevelPkgWithOwnComp = name: bevelPkgWithComp name name;

    in
    {
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

  bevelReleasePackages = mapAttrs (_: pkg: justStaticExecutables (doCheck pkg)) final.bevelHaskellPackages // {
    inherit (final) bevel-gather;
  };
  bevelRelease =
    final.symlinkJoin {
      name = "bevel-release";
      paths = attrValues final.bevelReleasePackages;
    };


  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super: final.bevelHaskellPackages
            );
      }
    );
}
