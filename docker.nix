{ nixpkgs ? import ./nix/pinnedNix.nix {} }:
let
  inherit (nixpkgs) pkgs runCommand;
  inherit (pkgs) busybox;

  project = pkgs.haskell.lib.justStaticExecutables
              ( import ./release.nix {} );
  resources = import ./resources.nix {};
in

pkgs.dockerTools.buildImage {
  name = "jeovazero/pepe-haskeller";
  tag = "latest";
  # docker images for heroku need bash/sh (contained in the busybox)
  contents = [ pkgs.cacert resources busybox ];
  created = "now";
  config = {
    Cmd = [ "${project}/bin/bot-poc" ];
    Env = [
      "BOT_RESOURCE_DIR=/data" # from resources.nix
      "BOT_OUTPUT_DIR=/home/caani/output"
    ];
  };
  runAsRoot = ''
      #!${pkgs.stdenv.shell}
      mkdir -p /home/caani/output
      chmod 777 /home/caani/output
  '';
}

