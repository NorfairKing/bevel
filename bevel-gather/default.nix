{ musl, stdenv, gitignoreSource }:
stdenv.mkDerivation {
  name = "bevel-gather";
  src = gitignoreSource ./.;
  buildInputs = [ musl ];
  buildCommand = ''
    mkdir -p $out/bin

    mkdir -p $out/bin
    musl-gcc \
      -Wall -Wextra -pedantic -O2 -s -static \
      $src/bevel-gather-before.c \
      -o $out/bin/bevel-gather-before
    musl-gcc \
      -Wall -Wextra -pedantic -O2 -s -static \
      $src/bevel-gather-after.c \
      -o $out/bin/bevel-gather-after

    mkdir -p $out/share
    ln -s $src/harnass.bash $out/share/
    ln -s $src/harnass.zsh $out/share
  '';
}
