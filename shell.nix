{ pkgs ? (import <nixpkgs> {}) }:

(import ./default.nix) {
  stdenv          = pkgs.stdenv;
  haskellPackages = pkgs.haskellngPackages;
}
