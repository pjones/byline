{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "f0574f297a95afc3755c9e9620e582871d4d4f6f";
  };

  nix-hs = import "${nix-hs-src}/default.nix" { inherit pkgs; };

in nix-hs {
  cabal = ./byline.cabal;
  flags = [ "build-examples" ];
}
