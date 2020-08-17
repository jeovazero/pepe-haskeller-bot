{ compiler ? "ghc865" }:

let

  pkgs = import ./nix/source.nix { json = ./nix/source.json; };

in
  pkgs.haskell.packages.${compiler}.callPackage ./default.nix {}

