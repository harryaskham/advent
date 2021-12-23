{ mkDerivation, array, base, benchpress, bimap, bitwise, containers
, extra, file-embed, fin, hs-functors, HUnit, lens, lib, linear
, MissingH, mod, monad-memo, mtl, parsec, pqueue, PSQueue
, QuickCheck, random, relude, safe, semirings, string-qq
, template-haskell, text, utility-ht, vector
}:
mkDerivation {
  pname = "x2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base benchpress bimap bitwise containers extra file-embed fin
    hs-functors lens linear MissingH mod monad-memo mtl parsec pqueue
    PSQueue random relude safe semirings string-qq template-haskell
    text utility-ht vector
  ];
  executableHaskellDepends = [
    array base benchpress bimap bitwise containers extra file-embed fin
    hs-functors lens linear MissingH mod monad-memo mtl parsec pqueue
    PSQueue random relude safe semirings string-qq template-haskell
    text utility-ht vector
  ];
  testHaskellDepends = [
    array base benchpress bimap bitwise containers extra file-embed fin
    hs-functors HUnit lens linear MissingH mod monad-memo mtl parsec
    pqueue PSQueue QuickCheck random relude safe semirings string-qq
    template-haskell text utility-ht vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
