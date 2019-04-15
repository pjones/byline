{ mkDerivation, ansi-terminal, base, colour, containers, exceptions
, haskeline, mtl, stdenv, terminfo-hs, text, transformers
}:
mkDerivation {
  pname = "byline";
  version = "0.3.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base colour containers exceptions haskeline mtl
    terminfo-hs text transformers
  ];
  homepage = "https://code.devalot.com/open/byline";
  description = "Library for creating command-line interfaces (colors, menus, etc.)";
  license = stdenv.lib.licenses.bsd2;
}
