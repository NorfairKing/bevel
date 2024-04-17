{ rustPlatform
}:
rustPlatform.buildRustPackage {
  pname = "bevel-select";
  version = "0.0.0";

  src = ./.;

  dontDisableStatic = true;

  cargoLock = {
    lockFile = ./Cargo.lock;
  };
}
