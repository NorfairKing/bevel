{ stdenv
, gitignoreSource
}:


stdenv.mkDerivation {
  name = "bevel-harness";
  src = gitignoreSource ./.;
  buildCommand = ''
    mkdir -p $out/share
    ln -s $src/harness.bash $out/share/
    ln -s $src/harness.zsh $out/share
  '';
}
