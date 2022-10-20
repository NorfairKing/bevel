{ mkDerivation, base, bevel-api, lib, servant, servant-auth-client
, servant-client, servant-client-core
}:
mkDerivation {
  pname = "bevel-client";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bevel-api servant servant-auth-client servant-client
    servant-client-core
  ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
