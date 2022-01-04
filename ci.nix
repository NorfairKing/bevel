let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgs.bevelRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "nixos-module-test" = import ./nix/nixos-module-test.nix {
    inherit sources;
    pkgs = pkgs;
  };
}
