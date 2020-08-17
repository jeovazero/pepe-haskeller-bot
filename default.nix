{ mkDerivation, aeson, base, bytestring, pretty-simple, req, stdenv
, text, transformers, xeno
}:
mkDerivation {
  pname = "bot-poc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring pretty-simple req text transformers xeno
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
