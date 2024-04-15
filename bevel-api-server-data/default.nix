{ mkDerivation, aeson, autodocodec, autoexporter, base, bevel-data
, esqueleto, lib, password, password-instances, persistent, text
, validity, validity-persistent, validity-text
}:
mkDerivation {
  pname = "bevel-api-server-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bevel-data esqueleto password
    password-instances persistent text validity validity-persistent
    validity-text
  ];
  libraryToolDepends = [ autoexporter ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
