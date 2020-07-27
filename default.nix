{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:
let
  # Files shared by all packags:
  linked = {
    "CHANGES.md" = ./CHANGES.md;
    "LICENSE" = ./LICENSE;
    "README.md" = ./README.md;
    "Setup.hs" = ./Setup.hs;
  };

  # Patch each package by replacing symbolic links with copies of the
  # actual files they link to.  This is necessary since Nix copies
  # each package to the nix store separately, thus breaking the links.
  postPatch = with pkgs.lib;
    concatMapStringsSep "\n"
      (file: ''
        if [ -L ${file} ]; then
          rm ${file}
          cp ${linked.${file}} ${file}
        fi
      '')
      (attrNames linked);
in
nix-hs {
  inherit postPatch;

  cabal = {
    byline = ./byline/byline.cabal;
  };

  flags = [ "build-examples" ];
  compiler = ghc;

  overrides = lib: self: super: {
    haskeline =
      if super ? haskeline_0_8_0_0 then
        lib.dontCheck super.haskeline_0_8_0_0
      else
        super.haskeline;
  };
}
