{ compiler ? "ghc884", nixpkgs ? import ./nix/pinnedNix.nix {} }:

let
  inherit (nixpkgs) pkgs;
  caaniRepo = builtins.fetchGit {
    "url" = "git@github.com:jeovazero/caani.git";
    "rev" = "6807b7e19ff2315af406c053805861e9fbdad252";
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