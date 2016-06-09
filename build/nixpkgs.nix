with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "byline";

  buildInputs = [
    # GHC:
    haskell.packages.lts-5_15.ghc

    # Non-Haskell Dependencies:
    zlib      # For the header files.
    zlib.out  # For the .so file.
    gnupg     # For signing packages.
    ncurses
  ];
}
