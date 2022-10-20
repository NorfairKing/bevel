{ mkDerivation, aeson, autodocodec, base, lib, text, validity
, validity-path, validity-text
}:
mkDerivation {
  pname = "bevel-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base text validity validity-path validity-text
  ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
