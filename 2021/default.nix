{ mkDerivation, array, base, bimap, containers, extra, HUnit, lib
, mod, mtl, parsec, pqueue, QuickCheck, relude, semirings, text
, vector
}:
mkDerivation {
  pname = "x2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bimap containers extra mod mtl parsec pqueue relude
    semirings text vector
  ];
  executableHaskellDepends = [
    array base bimap containers extra mod mtl parsec pqueue relude
    semirings text vector
  ];
  testHaskellDepends = [
    array base bimap containers extra HUnit mod mtl parsec pqueue
    QuickCheck relude semirings text vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
