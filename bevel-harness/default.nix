{ stdenv
}:

stdenv.mkDerivation {
  name = "bevel-harness";
  src = ./.;
  buildCommand = ''
    mkdir -p $out/share
    ln -s $src/harness.bash $out/share/
    ln -s $src/harness.zsh $out/share/
    ln -s $src/bindings.bash $out/share/
    ln -s $src/bindings.zsh $out/share/
  '';
}
