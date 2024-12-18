{ mkDerivation, array, base, benchpress, bimap, bitwise, containers
, extra, file-embed, fin, hashable, HUnit, lens, lib, linear
, MissingH, mod, monad-memo, mtl, parsec, pqueue, QuickCheck
, random, relude, safe, semirings, string-qq, template-haskell
, text, unordered-containers, utility-ht, vector
}:
mkDerivation {
  pname = "x2024";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base benchpress bimap bitwise containers extra file-embed fin
    hashable lens linear MissingH mod monad-memo mtl parsec pqueue
    random relude safe semirings string-qq template-haskell text
    unordered-containers utility-ht vector
  ];
  executableHaskellDepends = [
    array base benchpress bimap bitwise containers extra file-embed fin
    hashable lens linear MissingH mod monad-memo mtl parsec pqueue
    random relude safe semirings string-qq template-haskell text
    unordered-containers utility-ht vector
  ];
  testHaskellDepends = [
    array base benchpress bimap bitwise containers extra file-embed fin
    hashable HUnit lens linear MissingH mod monad-memo mtl parsec
    pqueue QuickCheck random relude safe semirings string-qq
    template-haskell text unordered-containers utility-ht vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
