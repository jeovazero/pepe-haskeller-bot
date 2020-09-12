{ mkDerivation, aeson, base, bytestring, req, stdenv, text, xeno }:
mkDerivation {
  pname = "bot-poc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ aeson base bytestring req text xeno ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
