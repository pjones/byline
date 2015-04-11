{ pkgs ? (import <nixpkgs> {}) }:

let haskellPackages = pkgs.haskellPackages; in

haskellPackages.cabal.mkDerivation (self: {
  pname = "byline";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;

  buildTools = with pkgs; [
    haskellPackages.ghc
    haskellPackages.cabalInstall
    haskellPackages.hlint
  ];

  buildDepends = with pkgs; [
    zlib
  ];

  shellHook = with pkgs; ''
    export LD_LIBRARY_PATH="${zlib}/lib:$LD_LIBRARY_PATH"
  '';

  meta = with self.stdenv.lib; {
    homepage = http://github.com/pjones/byline;
    description = "";
    license = licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
