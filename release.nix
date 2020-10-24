{ compiler ? "ghc884", nixpkgs ? import ./nix/pinnedNix.nix {} }:

let
  inherit (nixpkgs) pkgs;
  caaniRepo = builtins.fetchGit {
    "url" = "git@github.com:jeovazero/caani.git";
    "rev" = "d1a1cd6ea545703cb6aa31601e449a4dcc330f33";
  };
  caani = import "${caaniRepo}/release.nix" {};
  myPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      caani = caani;
    };
  };
  bot = myPkgs.callCabal2nix "bot-poc" ./. {};
in
  bot