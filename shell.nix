{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs =
    (with pkgs.haskell.packages.ghc948; [
      haskell-language-server
      ghc
      cabal-install
      implicit-hie
    ]) ++ (with pkgs; [
      fontconfig
      expat
      pkg-config
      SDL2
      harfbuzz
      icu
      glib
      pcre2
      espeak-ng
      libsndfile
      lzma
      libz
    ]);
}
