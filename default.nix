{ pkgs ? import <nixpkgs> { }
}:

let
  byline = import ./nix/cabal2nix.nix { inherit pkgs; };
  haskell = pkgs.haskellPackages;

in haskell.callPackage "${byline}" { }
