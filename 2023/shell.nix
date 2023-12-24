{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, adlude, array, base, base-unicode-symbols
      , benchpress, bimap, bitwise, containers, extra, file-embed, fin
      , hashable, HUnit, lens, lib, linear, linearEqSolver, megaparsec
      , mfsolve, MissingH, mod, monad-memo, mtl, parsec, pqueue
      , QuickCheck, random, relude, replace-megaparsec, safe, semirings
      , string-qq, template-haskell, text, unordered-containers
      , utility-ht, vector
      }:
      mkDerivation {
        pname = "x2023";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          adlude array base base-unicode-symbols benchpress bimap bitwise
          containers extra file-embed fin hashable lens linear linearEqSolver
          megaparsec mfsolve MissingH mod monad-memo mtl parsec pqueue random
          relude replace-megaparsec safe semirings string-qq template-haskell
          text unordered-containers utility-ht vector
        ];
        executableHaskellDepends = [
          adlude array base base-unicode-symbols benchpress bimap bitwise
          containers extra file-embed fin hashable lens linear linearEqSolver
          megaparsec mfsolve MissingH mod monad-memo mtl parsec pqueue random
          relude replace-megaparsec safe semirings string-qq template-haskell
          text unordered-containers utility-ht vector
        ];
        testHaskellDepends = [
          adlude array base base-unicode-symbols benchpress bimap bitwise
          containers extra file-embed fin hashable HUnit lens linear
          linearEqSolver megaparsec mfsolve MissingH mod monad-memo mtl
          parsec pqueue QuickCheck random relude replace-megaparsec safe
          semirings string-qq template-haskell text unordered-containers
          utility-ht vector
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
