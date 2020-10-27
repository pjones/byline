{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./byline.cabal;

  flags = [ "build-examples" ];
  compiler = ghc;

  overrides = lib: self: super: {
    haskeline =
      if super ? haskeline_0_8_1_0
      then lib.dontCheck super.haskeline_0_8_1_0
      else super.haskeline;

    ansi-terminal =
      if super ? ansi-terminal_0_11
      then super.ansi-terminal_0_11
      else super.ansi-terminal;

    optparse-applicative =
      if super ? optparse-applicative_0_16_0_0
      then super.optparse-applicative_0_16_0_0
      else super.optparse-applicative;
  };
}
