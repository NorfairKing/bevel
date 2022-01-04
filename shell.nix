let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "bevel-nix-shell";
  buildInputs = with pkgs; [
    haskellPackages.autoexporter
    (import sources.niv { inherit pkgs; }).niv
    killall
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
