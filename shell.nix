{ pkgs ? import <nixpkgs> { }
}:

let
  byline = import ./default.nix { inherit pkgs; };

in

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.hlint
    haskellPackages.hasktags
    # cabal-dependency-licenses

    (haskellPackages.ghcWithPackages (hspkgs: [
      byline
      hspkgs.cabal-install
    ]))
  ];
}
