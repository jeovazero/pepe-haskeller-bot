{ compiler ? "ghc865" }:

let
  pkgs = import ./nix/source.nix { json = ./nix/source.json; };
  caaniRepo = builtins.fetchGit {
    "url" = "git@github.com:jeovazero/caani.git";
    "rev" = "d1a1cd6ea545703cb6aa31601e449a4dcc330f33";
  };
  myPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      caani = import "${caaniRepo}/release.nix" {};
    };
  };
  bot = myPkgs.callCabal2nix "bot-poc" ./. {};
in
  bot