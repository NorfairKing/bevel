{ mkDerivation, base, bevel-data, genvalidity, genvalidity-path
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, lib, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "bevel-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bevel-data genvalidity genvalidity-path genvalidity-text
  ];
  testHaskellDepends = [
    base bevel-data genvalidity-sydtest genvalidity-sydtest-aeson
    sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/bevel#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
