final: prev:
with final.lib;
with final.haskell.lib;
let
  staticCheck = pkg:
    if final.stdenv.hostPlatform.isMusl
    then
      pkg.overrideAttrs
        (old: {
          postInstall = (old.postInstall or "") + ''
            for b in $out/bin/*
            do
              if ldd "$b"
              then
                echo "ldd succeeded on $b, which may mean that it is not statically linked"
                exit 1
              fi
            done
          '';
        })
    else pkg;
in
{
  bevelRelease = final.symlinkJoin {
    name = "bevel-release";
    paths = builtins.attrValues final.bevelReleasePackages;
    passthru = final.bevelReleasePackages;
  };

  bevelReleasePackages =
    let
      enableStatic = pkg:
        if final.stdenv.hostPlatform.isMusl
        then
          overrideCabal (staticCheck pkg)
            (old: {
              configureFlags = (old.configureFlags or [ ]) ++ [
                "--ghc-option=-optl=-static"
                # Static
                "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${final.zlib.static}/lib"
                "--extra-lib-dirs=${final.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
                # for -ltinfo
                "--extra-lib-dirs=${(final.ncurses.override { enableStatic = true; })}/lib"
              ];
              enableSharedExecutables = false;
              enableSharedLibraries = false;
            })
        else pkg;
    in
    builtins.mapAttrs (_: pkg: justStaticExecutables (enableStatic pkg)) final.haskellPackages.bevelPackages // {
      inherit (final)
        bevel-gather
        bevel-harness
        bevel-select;
    };

  bevel-gather = staticCheck (final.callPackage ../bevel-gather/default.nix { });
  bevel-harness = final.callPackage ../bevel-harness/default.nix { };
  bevel-select = (staticCheck (final.callPackage ../bevel-select/default.nix { })).overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
      final.clippy
    ];
    preCheck = (old.preCheck or "") + ''
      cargo clippy --no-deps -- --forbid warnings
    '';
  });

  sqlite =
    if final.stdenv.hostPlatform.isMusl
    then prev.sqlite.overrideAttrs (_: { dontDisableStatic = true; })
    else prev.sqlite;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super:
          let
            bevelPkg = name:
              buildFromSdist (overrideCabal (self.callPackage (../${name}/default.nix) { }) (old: {
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
                doCheck = false; # Only for coverage
                hyperlinkSource = false;
                enableLibraryProfiling = false;
                enableExecutableProfiling = false;
                # Ugly hack because we can't just add flags to the 'test' invocation.
                # Show test output as we go, instead of all at once afterwards.
                testTarget = (old.testTarget or "") + " --show-details=direct";
              }));
            bevelPkgWithComp =
              exeName: name:
              self.generateOptparseApplicativeCompletions [ exeName ] (bevelPkg name);
            bevelPkgWithOwnComp = name: bevelPkgWithComp name name;

            bevelPackages = {
              bevel-api = bevelPkg "bevel-api";
              bevel-api-gen = bevelPkg "bevel-api-gen";
              bevel-api-server = bevelPkgWithOwnComp "bevel-api-server";
              bevel-api-server-gen = bevelPkg "bevel-api-server-gen";
              bevel-api-server-data = bevelPkg "bevel-api-server-data";
              bevel-api-server-data-gen = bevelPkg "bevel-api-server-data-gen";
              bevel-cli = addTestToolDepend (bevelPkgWithComp "bevel" "bevel-cli") final.bevel-gather;
              bevel-client = bevelPkg "bevel-client";
              bevel-client-data = bevelPkg "bevel-client-data";
              bevel-client-data-gen = bevelPkg "bevel-client-data-gen";
              bevel-data = bevelPkg "bevel-data";
              bevel-data-gen = bevelPkg "bevel-data-gen";
            };

            fixGHC = pkg:
              if final.stdenv.hostPlatform.isMusl
              then
                pkg.override
                  {
                    # To make sure that executables that need template
                    # haskell can be linked statically.
                    enableRelocatedStaticLibs = true;
                    enableShared = false;
                    enableDwarf = false;
                  }
              else pkg;
          in
          {
            ghc = fixGHC super.ghc;
            buildHaskellPackages = old.buildHaskellPackages.override (oldBuildHaskellPackages: {
              ghc = fixGHC oldBuildHaskellPackages.ghc;
            });

            inherit bevelPackages;

            servant-auth-server = dontCheck (unmarkBroken super.servant-auth-server);

            bevelRelease =
              final.symlinkJoin {
                name = "bevel-release";
                paths = final.lib.attrValues self.bevelPackages;
              };
          } // bevelPackages
      );
  });
}
