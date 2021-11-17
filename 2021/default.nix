{ mkDerivation, array, base, containers, lib, parsec, pqueue, text
, vector
}:
mkDerivation {
  pname = "x2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base containers parsec pqueue text vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
