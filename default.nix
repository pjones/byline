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
      if super ? haskeline_0_8_1_0 then
        lib.dontCheck super.haskeline_0_8_1_0
      else
        super.haskeline;
  };
}
