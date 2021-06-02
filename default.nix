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
    attoparsec = super.attoparsec_0_14_1;
    relude = super.relude_1_0_0_1;
  };
}
