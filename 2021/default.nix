{ mkDerivation, array, base, bimap, containers, extra, file-embed
, HUnit, lib, mod, mtl, parsec, pqueue, QuickCheck, relude
, semirings, template-haskell, text, vector
}:
mkDerivation {
  pname = "x2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bimap containers extra file-embed mod mtl parsec pqueue
    relude semirings template-haskell text vector
  ];
  executableHaskellDepends = [
    array base bimap containers extra file-embed mod mtl parsec pqueue
    relude semirings template-haskell text vector
  ];
  testHaskellDepends = [
    array base bimap containers extra file-embed HUnit mod mtl parsec
    pqueue QuickCheck relude semirings template-haskell text vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
