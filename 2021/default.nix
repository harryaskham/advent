{ mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "x2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
