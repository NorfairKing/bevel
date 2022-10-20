{ mkDerivation, base, bevel-api-server-data, bevel-data, lib
, persistent, persistent-sqlite, text, validity, validity-text
}:
mkDerivation {
  pname = "bevel-client-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bevel-api-server-data bevel-data persistent persistent-sqlite
    text validity validity-text
  ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
