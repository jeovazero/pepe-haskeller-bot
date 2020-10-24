{ nixpkgs ? import ./nix/pinnedNix.nix { } }:
let
  inherit (nixpkgs) pkgs;

  project = pkgs.haskell.lib.justStaticExecutables
              ( import ./release.nix {} );
  resources = import ./resources.nix {};
in

pkgs.dockerTools.buildImage {
  name = "jeovazero/pepe-haskeller";
  contents = [ pkgs.cacert resources ];
  created = "now";
  config = {
    Cmd = [ "${project}/bin/bot-poc" ];
  };
}

