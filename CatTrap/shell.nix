{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs =
    (with pkgs.haskell.packages.ghc948; [
      haskell-language-server
      ghc
      cabal-install
    ]) ++ (with pkgs; [
      fontconfig
      expat
      harfbuzz
      pkg-config
      icu
    ]);
}
