{ pkgs ? import <nixpkgs> { }
}:

pkgs.stdenvNoCC.mkDerivation rec {
  name = "byline.nix";
  src = pkgs.lib.cleanSource ../.;

  buildInputs = with pkgs; [
    cabal2nix
  ];

  buildCommand = ''
    cabal2nix -fbuild-examples ${src} > $out
  '';
}
