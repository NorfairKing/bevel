{ mkDerivation, base, bevel-api-server-data-gen, bevel-client-data
, bevel-data-gen, genvalidity, genvalidity-persistent
, genvalidity-sydtest, genvalidity-text, lib, sydtest
, sydtest-discover
}:
mkDerivation {
  pname = "bevel-client-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bevel-client-data bevel-data-gen genvalidity
    genvalidity-persistent genvalidity-text
  ];
  testHaskellDepends = [
    base bevel-api-server-data-gen bevel-client-data
    genvalidity-sydtest sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
