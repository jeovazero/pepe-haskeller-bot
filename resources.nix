{ nixpkgs ? import ./nix/source.nix { json = ./nix/source.json; } }:
nixpkgs.pkgs.stdenv.mkDerivation {
    name = "resources";
    src = ./resources;
    buildPhase = "mkdir -p $out/data";
    installPhase = ''
      ls -la
      cp . -r $out/data -v
    '';
}