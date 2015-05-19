{ stdenv, haskellPackages }:

let
  env = haskellPackages.ghcWithPackages (p: with p; [
    # Tools:
    cabal-install
    hlint

    # Libraries:
    ansi-terminal
    base
    colour
    exceptions
    haskeline
    mtl
    terminfo-hs
    text
    transformers
  ]);

in stdenv.mkDerivation rec {
  name = "byline";
  src = ./src;

  buildInputs = [ env ];

  buildPhase = ''
    ( HOME="$(mktemp -d)" # For cabal-install.
      if [ ! -d .cabal-sandbox ]; then
        cabal sandbox init
      fi

      cabal configure -fmaintainer -fbuild-examples
      cabal build || exit 1
    ) && hlint src
  '';

  installPhase = ''
  '';

  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
