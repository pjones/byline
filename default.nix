{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs = import (fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "136d1a5c1e87c2ef5e8050c6b521f4d529645eba";
    ref = "next";
  }) { inherit pkgs; };

in nix-hs {
  cabal = ./byline.cabal;
  flags = [ "build-examples" ];
}
