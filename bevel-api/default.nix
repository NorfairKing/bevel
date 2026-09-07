{ mkDerivation, aeson, appendful, autodocodec, base
, bevel-api-server-data, bevel-client-data, bevel-data, containers
, lib, persistent, servant, servant-auth, servant-auth-server, text
, validity, validity-text
}:
mkDerivation {
  pname = "bevel-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson appendful autodocodec base bevel-api-server-data
    bevel-client-data bevel-data containers persistent servant
    servant-auth servant-auth-server text validity validity-text
  ];
  homepage = "https://github.com/NorfairKing/bevel-api-cli-login#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
