{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, benchpress, bimap, bitwise
      , containers, extra, file-embed, fin, hs-functors, HUnit, lens, lib
      , linear, MissingH, mod, monad-memo, mtl, parallel, parsec, pqueue
      , PSQueue, QuickCheck, random, relude, safe, semirings, string-qq
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
          hs-functors lens linear MissingH mod monad-memo mtl parallel parsec
          pqueue PSQueue random relude safe semirings string-qq
          template-haskell text utility-ht vector
        ];
        executableHaskellDepends = [
          array base benchpress bimap bitwise containers extra file-embed fin
          hs-functors lens linear MissingH mod monad-memo mtl parallel parsec
          pqueue PSQueue random relude safe semirings string-qq
          template-haskell text utility-ht vector
        ];
        testHaskellDepends = [
          array base benchpress bimap bitwise containers extra file-embed fin
          hs-functors HUnit lens linear MissingH mod monad-memo mtl parallel
          parsec pqueue PSQueue QuickCheck random relude safe semirings
          string-qq template-haskell text utility-ht vector
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
