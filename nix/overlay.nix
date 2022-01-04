final: previous:
with final.haskell.lib;

{
  bevel-gather = final.callPackage ../bevel-gather { };
  bevelHaskellPackages =
    let
      bevelPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              buildStrictly (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
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
      "bevel-cli" = bevelPkgWithComp "bevel" "bevel-cli";
      "bevel-client" = bevelPkg "bevel-client";
      "bevel-client-data" = bevelPkg "bevel-client-data";
      "bevel-data" = bevelPkg "bevel-data";
      "bevel-data-gen" = bevelPkg "bevel-data-gen";
    };

  bevelRelease =
    final.symlinkJoin {
      name = "bevel-release";
      paths = builtins.map justStaticExecutables (final.lib.attrValues final.bevelPackages);
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
