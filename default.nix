{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, ghc ? "default" }:

nix-hs {
  cabal = ./byline.cabal;
  flags = [ "build-examples" ];
  compiler = ghc;
}
