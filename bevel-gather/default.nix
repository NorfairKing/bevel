{ gcc, stdenv }:
stdenv.mkDerivation {
  name = "bevel-gather";
  src = ./bevel.c;
  buildCommand = ''
    mkdir -p $out/bin
    gcc \
      -Wall -Wextra -pedantic \
      $src \
      -o $out/bin/bevel-gather
  '';
}
