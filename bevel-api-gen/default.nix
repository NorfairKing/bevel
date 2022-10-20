{ mkDerivation, base, bevel-api, bevel-api-server-data-gen
, genvalidity, genvalidity-appendful, genvalidity-sydtest
, genvalidity-text, lib, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "bevel-api-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bevel-api bevel-api-server-data-gen genvalidity
    genvalidity-appendful genvalidity-text
  ];
  testHaskellDepends = [
    base bevel-api genvalidity-sydtest sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/bevel-api-cli-login#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
