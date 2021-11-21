{ mkDerivation, array, base, containers, extra, HUnit, lib, mtl
, parsec, pqueue, QuickCheck, relude, text, vector
}:
mkDerivation {
  pname = "x2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base containers extra mtl parsec pqueue relude text vector
  ];
  executableHaskellDepends = [
    array base containers extra mtl parsec pqueue relude text vector
  ];
  testHaskellDepends = [
    array base containers extra HUnit mtl parsec pqueue QuickCheck
    relude text vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
