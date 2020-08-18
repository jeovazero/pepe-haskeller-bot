{ nixpkgs ? import ./nix/source.nix { json = ./nix/source.json; }
 }:
let
  inherit (nixpkgs) pkgs;

  project = pkgs.haskell.lib.justStaticExecutables
              ( pkgs.haskellPackages.callPackage ./default.nix { } );
in

pkgs.dockerTools.buildImage {
  name = "docker-bot-poc";
  contents = [ pkgs.cacert ];
  created = "now";
  config = {
    Cmd = [ "${project}/bin/bot-poc" ];
  };
}

