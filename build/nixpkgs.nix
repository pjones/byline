with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "byline";

  buildInputs = [
    haskell.packages.lts-4_2.ghc
    ncurses
  ];
}
