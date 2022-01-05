{ sources ? import ./sources.nix }:
let
  pre-commit-hooks = import sources.pre-commit-hooks;
in
{
  run = pre-commit-hooks.run {
    src = ../.;
    hooks = {
      hlint.enable = true;
      hpack.enable = true;
      nixpkgs-fmt.enable = true;
      ormolu.enable = true;
      clang-format = {
        enable = true;
        types_or = [ "c" ];
      };
    };
  };
  tools = with pre-commit-hooks; [
    hlint
    hpack
    nixpkgs-fmt
    ormolu
  ];
}
