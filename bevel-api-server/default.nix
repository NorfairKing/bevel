{ mkDerivation, appendful-persistent, autodocodec, autodocodec-yaml
, base, bevel-api, bevel-api-server-data, bevel-data, envparse
, jose, lib, monad-logger, mtl, optparse-applicative, password
, path, path-io, persistent, persistent-sqlite, servant-auth-server
, servant-server, text, wai, warp, yaml
}:
mkDerivation {
  pname = "bevel-api-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    appendful-persistent autodocodec autodocodec-yaml base bevel-api
    bevel-api-server-data bevel-data envparse jose monad-logger mtl
    optparse-applicative password path path-io persistent
    persistent-sqlite servant-auth-server servant-server text wai warp
    yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "bevel-api-server";
}
