{ pkgs ? (import <nixpkgs> {})
, ghc  ? "7.8.4" # Allow overriding the GHC version.

# Logic to pick the correct version of haskellPackages:
, haskellPackages ? if ghc == "7.8.4"
                      then pkgs.haskell.packages.ghc784
                      else pkgs.haskell.packages.ghc7101
}:

(import ./default.nix) {
  stdenv          = pkgs.stdenv;
  haskellPackages = haskellPackages;
}
