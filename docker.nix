{ nixpkgs ? import ./nix/pinnedNix.nix { system-features = "kvm"; } }:
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
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    mkdir -p /output
  '';
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

