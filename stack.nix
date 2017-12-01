{ nixpkgs ? import <nixpkgs> { }
, ghc ? nixpkgs.ghc
}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "byline";

  buildInputs = [
    ncurses
  ];

  inherit ghc;
}
