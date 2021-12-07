{ mkDerivation, array, base, bimap, containers, criterion, extra
, file-embed, hs-functors, HUnit, lib, mod, monad-memo, mtl, parsec
, pqueue, QuickCheck, relude, semirings, template-haskell, text
, utility-ht, vector
}:
mkDerivation {
  pname = "x2021";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bimap containers criterion extra file-embed hs-functors
    mod monad-memo mtl parsec pqueue relude semirings template-haskell
    text utility-ht vector
  ];
  executableHaskellDepends = [
    array base bimap containers criterion extra file-embed hs-functors
    mod monad-memo mtl parsec pqueue relude semirings template-haskell
    text utility-ht vector
  ];
  testHaskellDepends = [
    array base bimap containers criterion extra file-embed hs-functors
    HUnit mod monad-memo mtl parsec pqueue QuickCheck relude semirings
    template-haskell text utility-ht vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
