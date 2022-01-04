{ sources ? import ./sources.nix
}:
let
  pkgsf = import sources.nixpkgs;
in
pkgsf {
  overlays =
    [
      (import (sources.appendful + "/nix/overlay.nix"))
      (import (sources.mergeful + "/nix/overlay.nix"))
      (import (sources.mergeless + "/nix/overlay.nix"))
      (import (sources.safe-coloured-text + "/nix/overlay.nix"))
      (import (sources.validity + "/nix/overlay.nix"))
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
