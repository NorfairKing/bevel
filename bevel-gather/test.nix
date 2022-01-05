{ musl
, sqlite
, gnutar
, stdenv
, gitignoreSource
, zsh
}:
let
  gather = import ./default.nix {
    inherit
      musl
      sqlite
      gnutar
      stdenv
      gitignoreSource;
  };
in
stdenv.mkDerivation {
  name = "bevel-gather-test";
  buildInputs = [
    gather
    zsh
  ];
  buildCommand = ''
    set -x

    # Convince ZSH to use the harnass as its .zshrc file
    mkdir zdotdir
    export ZDOTDIR=$(pwd)/zdotdir
    ln -s ${./harnass.zsh} zdotdir/.zshrc
    ls -la

    zsh -ic '_bevel_preexec'

    echo hi > $out

    set +x
  '';
}
