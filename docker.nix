{ nixpkgs ? import ./nix/pinnedNix.nix {} }:
let
  inherit (nixpkgs) pkgs runCommand;

  project = pkgs.haskell.lib.justStaticExecutables
              ( import ./release.nix {} );
  resources = import ./resources.nix {};
  outputDir = runCommand "outputDir" {} ''
    mkdir -p $out/output  
  '';
in

pkgs.dockerTools.buildImage {
  name = "jeovazero/pepe-haskeller";
  contents = [ pkgs.cacert resources outputDir ];
  created = "now";
  config = {
    Cmd = [ "${project}/bin/bot-poc" ];
    Env = [
      "BOT_RESOURCE_DIR=/data" # from resources.nix
      "BOT_OUTPUT_DIR=/output"
    ];
    Volumes = {
      "/output" = {};
    };
  };
}

