{ mkDerivation, base, bevel-api, bevel-api-gen, bevel-api-server
, bevel-api-server-data, bevel-client, bevel-data-gen, cookie
, genvalidity-sydtest, http-client, http-types, lib, monad-logger
, opt-env-conf-test, persistent-sqlite, QuickCheck, servant
, servant-auth-server, sydtest, sydtest-discover, text, warp
}:
mkDerivation {
  pname = "bevel-api-server-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bevel-api bevel-api-gen bevel-api-server bevel-api-server-data
    bevel-client bevel-data-gen cookie genvalidity-sydtest http-client
    http-types monad-logger persistent-sqlite QuickCheck servant
    servant-auth-server sydtest text warp
  ];
  testHaskellDepends = [
    base bevel-api bevel-api-gen bevel-api-server bevel-api-server-data
    bevel-client bevel-data-gen cookie genvalidity-sydtest http-client
    http-types monad-logger opt-env-conf-test persistent-sqlite
    QuickCheck servant servant-auth-server sydtest text warp
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
