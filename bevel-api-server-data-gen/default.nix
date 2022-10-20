{ mkDerivation, base, bevel-api-server-data, bevel-data-gen
, bytestring, genvalidity, genvalidity-persistent
, genvalidity-sydtest, genvalidity-text, lib, password, QuickCheck
, sydtest, sydtest-discover, sydtest-persistent-sqlite
}:
mkDerivation {
  pname = "bevel-api-server-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bevel-api-server-data bevel-data-gen bytestring genvalidity
    genvalidity-persistent genvalidity-text password QuickCheck
  ];
  testHaskellDepends = [
    base bevel-api-server-data genvalidity-sydtest sydtest
    sydtest-persistent-sqlite
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
