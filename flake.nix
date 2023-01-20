{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    haskellrc.url = "github:pjones/haskellrc";
    haskellrc.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      # The name of the Haskell package:
      packageName = "byline";

      # Haskell package overrides:
      packageOverrides = haskell: { };

      # List of supported compilers:
      supportedCompilers = [
        "ghc8107"
        "ghc902"
        "ghc925"
        "ghc944"
      ];

      # List of supported systems:
      supportedSystems = [ "x86_64-linux" ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

      # A source file list cleaner for Haskell programs:
      haskellSourceFilter = src:
        nixpkgs.lib.cleanSourceWith {
          inherit src;
          filter = name: type:
            let baseName = baseNameOf (toString name); in
            nixpkgs.lib.cleanSourceFilter name type &&
            !(
              baseName == "dist-newstyle"
              || nixpkgs.lib.hasPrefix "." baseName
            );
        };

      # The package derivation:
      derivation = haskell:
        haskell.callCabal2nix
          packageName
          (haskellSourceFilter ./.)
          (packageOverrides haskell);
    in
    {
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        {
          # The full Haskell package for the default compiler:
          ${packageName} = derivation pkgs.haskellPackages;

          # Just the executables for the default compiler:
          default = pkgs.haskell.lib.justStaticExecutables (derivation pkgs.haskellPackages);
        } // builtins.listToAttrs (map
          (compiler: {
            name = "${packageName}-${compiler}";
            value = derivation pkgs.haskell.packages.${compiler};
          })
          supportedCompilers));

      devShells = forAllSystems (system: {
        default = nixpkgsFor.${system}.haskellPackages.shellFor {
          NIX_PATH = "nixpkgs=${nixpkgsFor.${system}.path}";

          packages = _: [ self.packages.${system}.${packageName} ];
          withHoogle = true;
          buildInputs = with nixpkgsFor.${system}; [
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            inputs.haskellrc.packages.${system}.default
          ];
        };
      });
    };
}
