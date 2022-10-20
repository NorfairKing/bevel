{ mkDerivation, aeson, appendful, autodocodec, base
, bevel-api-server-data, bevel-client-data, bevel-data, lib
, persistent, servant, servant-auth, servant-auth-server, text
, validity, validity-text
}:
mkDerivation {
  pname = "bevel-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson appendful autodocodec base bevel-api-server-data
    bevel-client-data bevel-data persistent servant servant-auth
    servant-auth-server text validity validity-text
  ];
  homepage = "https://github.com/NorfairKing/bevel-api-cli-login#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
