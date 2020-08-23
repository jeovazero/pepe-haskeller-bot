{ nixpkgs ? import ./nix/source.nix { json = ./nix/source.json; }
 }:
let
  inherit (nixpkgs) pkgs;

  project = pkgs.haskell.lib.justStaticExecutables
              ( pkgs.haskellPackages.callPackage ./default.nix { } );
in

pkgs.dockerTools.buildImage {
  name = "jeovazero/pepe-haskeller";
  contents = [ pkgs.cacert ];
  created = "now";
  config = {
    Cmd = [ "${project}/bin/bot-poc" ];
  };
}

