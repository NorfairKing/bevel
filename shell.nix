{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "bevel-nix-shell";
  buildInputs = with pkgs; [
    (import sources.niv { inherit pkgs; }).niv
    bevel-gather
    haskellPackages.autoexporter
    killall
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
