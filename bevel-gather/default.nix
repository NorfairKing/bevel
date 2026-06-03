{ musl
, pkgsStatic
, stdenv
}:


stdenv.mkDerivation {
  name = "bevel-gather";
  src = ./.;
  buildInputs = [ musl pkgsStatic.sqlite ];
  buildCommand = ''
    mkdir -p $out/bin
    musl-gcc \
      -Wall -Wextra -pedantic -O2 -s -static -Wl,--gc-sections -Wl,--strip-all \
      $src/bevel-gather.c \
      -I${pkgsStatic.sqlite.dev}/include \
      ${pkgsStatic.sqlite.out}/lib/libsqlite3.a \
      -lpthread \
      -o $out/bin/bevel-gather

    ldd $out/bin/bevel-gather || true
    du -h $out/bin/bevel-gather
  '';
}
