{ rustPlatform
}:
rustPlatform.buildRustPackage {
  pname = "bevel-select";
  version = "0.0.0";

  src = ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
  };

  # ...
}
