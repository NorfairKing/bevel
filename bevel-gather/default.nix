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
      $src/bevel-gather-before.c \
      sqlite3.c \
      -o $out/bin/bevel-gather-before
    musl-gcc \
      -Wall -Wextra -pedantic -O2 -s -static -Wl,--gc-sections -Wl,--strip-all \
      $src/bevel-gather-after.c \
      sqlite3.c \
      -o $out/bin/bevel-gather-after

    strip --strip-unneeded $out/bin/bevel-gather-before
    strip --strip-unneeded $out/bin/bevel-gather-after
    ldd $out/bin/bevel-gather-before || true
    ldd $out/bin/bevel-gather-after || true
    du -h $out/bin/bevel-gather-before
    du -h $out/bin/bevel-gather-after

    mkdir -p $out/share
    ln -s $src/harnass.bash $out/share/
    ln -s $src/harnass.zsh $out/share
  '';
}
