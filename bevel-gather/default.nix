{ musl
, sqlite
, gnutar
, stdenv
, gitignoreSource
}:


stdenv.mkDerivation {
  name = "bevel-gather";
  src = gitignoreSource ./.;
  buildInputs = [ musl gnutar ];
  nativeBuildInputs = [ sqlite.dev ];
  buildCommand = ''
    mkdir -p $out/

    tar xzf ${sqlite.src} --one-top-level=sqlite
    ln -s sqlite/*/sqlite3.c sqlite3.c

    mkdir -p $out/bin
    musl-gcc \
      -Wall -Wextra -pedantic -O2 -s -static -Wl,--gc-sections -Wl,--strip-all \
      $src/bevel-gather.c \
      sqlite3.c \
      -o $out/bin/bevel-gather

    ldd $out/bin/bevel-gather || true
    du -h $out/bin/bevel-gather
  '';
}
